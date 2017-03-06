#' update response matrix
#'
#' update the globally \code{response matrix} and the unglobally normalized
#' response matrix \code{door_response_matrix_non_normalized} by introducing new
#' consensus response data of given receptor.
#'
#' The merging sequence could be arranged by the routine process (using
#' \code{\link{model_response}} or taking the optimized sequence that is chosen from
#' permutations. The mean correlation between merged responses and each original
#' recording will be computed for each permutation, the optimozed sequence is
#' with the highest correlation.
#'
#' @param receptor character string, name of given odorant receptor.
#' @param permutation logical, if TRUE, the sequence is chosen from permutation,
#'   if FALSE, sequence is chosen by the routine process.
#' @param perm a matrix with one sequence of study names per row, if empty, all
#'   possible permutations of study names will be provided.
#' @param response_matrix_nn data frame, response data that has not been
#'   globally normalized.
#' @param response_matrix data frame, globally normalized response data.
#' @param responseRange data frame, response range of studies.
#' @param weightGlobNorm data frame, weight matrix for global normalization.
#' @param overlapValues minimum overlap between studies to perfom a merge
#' @param select.MDValue the minimum mean distance between studies to perfom a
#'   merge (used if permutation == FALSE or if permutation == TRUE AND strict ==
#'   TRUE)
#' @param door_excluded_data the data frame that contains the list of excluded data
#'   sets.
#' @param plot logical
#' @param strict logical, if TRUE merging a permutation will be stopped once a
#'   single merge has a mean distance above select.MDValue (only valid if
#'   permutation == TRUE)
#'
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Shouwen Ma <\email{daniel.muench@@uni-konstanz.de}>
#' @seealso \code{\link{model_response}},\code{\link{model_response_seq}}
#' @keywords data
#' @export
#' @examples
#' \dontrun{
#' # load data
#' library(DoOR.data)
#' load_door_data()
#' # update the entry "Or67b" of data "door_response_matrix" and
#' # "door_response_matrix_non_normalized" with permutations.
#'  update_door_database(receptor="Or67b", permutation = TRUE)
#' }
update_door_database <- function(receptor,
                           permutation = TRUE,
                           perm,
                           response_matrix_nn = door_default_values("door_response_matrix_non_normalized"),
                           response_matrix    = door_default_values("door_response_matrix"),
                           responseRange      = door_default_values("door_response_range"),
                           weightGlobNorm     = door_default_values("door_global_normalization_weights"),
                           select.MDValue     = door_default_values("select.MDValue"),
                           strict             = TRUE,
                           overlapValues      = door_default_values("overlapValues"),
                           door_excluded_data      = door_default_values("door_excluded_data"),
                           plot = FALSE) {
  da            <- get(receptor)
  recordColumn  <- as.numeric( c((door_default_values("num.charColumns")+1):dim(da)[2]) )
  studies       <- names(da)[recordColumn]

  if (permutation == TRUE) {
    da            <- filter_data(da, overlapValues = overlapValues)
    excluded      <- as.character(da$excluded$study)
    da            <- da$data
    recordColumn  <- as.numeric( c((door_default_values("num.charColumns")+1):dim(da)[2]) )
    studies       <- names(da)[recordColumn]

    if(length(da) > door_default_values("num.charColumns")) {

      if (missing(perm)) {
        perm <- matrix(studies[permutations(length(studies))], ncol=length(studies))
        message(paste("All possible permutations (", dim(perm)[1], ") have been calculated, now merging.", sep = ""))
      }


      # compute the mean correlation between merged responses and original response.
      meanCorrel <- matrix(NA,nrow=dim(perm)[1])

      for (i in 1:dim(perm)[1]) {
        merge.try <- try(model_response_seq(data=da, SEQ=perm[i,], overlapValues = overlapValues, select.MDValue = select.MDValue, strict = strict, plot = plot),silent = TRUE)
        if (inherits(merge.try, "try-error")) {
          meanCorrel_merge.try <- NA
        } else {
          meanCorrel_merge.try <- mean(unlist(sapply(da[,recordColumn],function(x) calculate_model(merge.try, x)[[1]]["MD"])))
        }
        meanCorrel[i,] <- meanCorrel_merge.try

        message(paste("[",i,"/",dim(perm)[1],"] ",paste(perm[i,], collapse = ", "), " ------ Mean distance: ", round(meanCorrel_merge.try, 4), sep = ""))
      }

      if (all(is.na(meanCorrel)))
        stop("No good sequence found")

      message("--------------------------------------------------------")

      perm_MC <- data.frame(perm,meanCorrel) 	# data frame, the last column contains the mean correlation values.

      # find and show the sequence with the lowest MD
      min.MD  <- which.min(meanCorrel)
      perm     <- perm[min.MD[1],]

      message(paste("The optimized sequence with the lowest mean MD", round(meanCorrel[min.MD], 4), "is:"))
      message(paste(perm, collapse = " -> "))

      # merge response data with the optimized sequence.
      merge <- model_response_seq(data = da, SEQ = perm, overlapValues = overlapValues, plot = plot)
    } else {
      merge <- rep(NA, dim(da)[1])
      message("No data left to merge, returning NAs")
    }

  } else { # END if (permutation == TRUE)
    merge <- model_response(da, glob.normalization = FALSE, select.MDValue = select.MDValue, overlapValues = overlapValues, plot = plot)
    excluded <- merge$door_excluded_data
    merge <- merge$model.response[,"merged_data"]
  }

  # update  door_response_matrix_non_normalized
  merged_data_withInChIKey <- data.frame(InChIKey = da$InChIKey, merged_data = merge)
  matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(response_matrix_nn))
  findNA_InChIKey <- which(is.na(matchInChIKey))
  if (!is.na(findNA_InChIKey[1])) {
    addRow <- matrix(NA, nrow=length(findNA_InChIKey), ncol= dim(response_matrix_nn)[2])
    colnames(addRow) <- colnames(response_matrix_nn)
    rownames(addRow) <- merged_data_withInChIKey[findNA_InChIKey,"InChIKey"]
    response_matrix_nn <- rbind(response_matrix_nn,addRow)
    response_matrix <- rbind(response_matrix,addRow)
    matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(response_matrix_nn))
  }
  response_matrix_nn[matchInChIKey, receptor] <- merge
  assign("door_response_matrix_non_normalized", response_matrix_nn, envir = .GlobalEnv)
  message(paste("door_response_matrix_non_normalized has been updated for",receptor))

  # update door_response_matrix
  name.Stud    <- colnames(da)[recordColumn]
  mp_orx       <- match(colnames(da)[recordColumn], responseRange[,"study"])
  Rmax         <- apply(as.matrix(da[,recordColumn]),2,function(x) max(range(x,na.rm=TRUE)))
  Smax         <- responseRange[mp_orx,"max"]
  merged_data  <- global_norm(RMAX = Rmax,SMAX = Smax, MV = merge, name.Stud = name.Stud, responseRange = responseRange, weightGlobNorm = weightGlobNorm)
  merged_data_withInChIKey <- data.frame(InChIKey = da$InChIKey, merged_data = merged_data)
  matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(response_matrix))
  response_matrix[matchInChIKey, receptor] <- merged_data

  assign("door_response_matrix", response_matrix, envir = .GlobalEnv)
  message(paste("door_response_matrix has been updated for",receptor))

  # update door_response_matrix.excluded
  door_excluded_data$excluded <- as.character(door_excluded_data$excluded)
  door_excluded_data[door_excluded_data$OR == receptor, "excluded"] <- paste(excluded, collapse = ", ")
  assign("door_excluded_data", door_excluded_data, envir = .GlobalEnv)
  message(paste("door_excluded_data has been updated for",receptor))

}
