#' update response matrix
#' 
#' update the globally \code{response matrix} and the unglobally normalized
#' response matrix \code{response.matrix_non.normalized} by introducing new
#' consensus response data of given receptor.
#' 
#' The merging sequence could be arranged by the routine process (using
#' \code{\link{modelRP}} or taking the optimized sequence that is chosen from
#' permutations. The mean correlation between merged responses and each
#' original recording will be computed for each permutation, the optimozed
#' sequence is with the highest correlation.
#' 
#' @param receptor character string; name of given odorant receptor.
#' @param permutation logical; if TRUE, the sequence is chosen from
#' permutation, if FALSE, sequence is chosen by the routine process.
#' @param perm a matrix with one sequence of study names per row, if empty, all possible permutations of study names will be provided.
#' @param responseMatrix_nn data frame; response data that has not
#' been globally normalized.
#' @param responseMatrix data frame; globally normalized response data.
#' @param responseRange data frame; response range of studies.
#' @param weightGlobNorm data frame; weight matrix for global normalization.
#' @param overlapValues minimum overlap between studies to perfom a merge
#' @param select.MDValue the minimum mean distance between studies to perfom a merge (only valid for permutation == F)
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Shouwen Ma <\email{daniel.muench@@uni-konstanz.de}>
#' @seealso \code{\link{modelRP}},\code{\link{modelRPSEQ}}
#' @keywords data
#' @examples
#' \dontrun{
#' library(DoOR.data)
#' loadRD()
#' # update the entry "Or67b" of data "response.matrix" and "response.matrix_non.normalized" with permutations.
#' # updateDatabase(receptor="Or67b", permutation = TRUE)
#' }
updateDatabase <- function(receptor, 
                           permutation = TRUE, 
                           perm, 
                           responseMatrix_nn = default.val("response.matrix_non.normalized"), 
                           responseMatrix = default.val("response.matrix"), 
                           responseRange = default.val("response.range"), 
                           weightGlobNorm = default.val("weight.globNorm"),
                           select.MDValue=default.val("select.MDValue"), 
                           overlapValues = default.val("overlapValues"),
                           excluded.data = default.val("excluded.data"),
                           plot = F) {	
  da            <- get(receptor)
  recordColumn  <- as.numeric( c((default.val("num.charColumns")+1):dim(da)[2]) )
  studies       <- names(da)[recordColumn]
  
  if (permutation == TRUE) {
    da            <- filterData(da, overlapValues = overlapValues)
    excluded      <- as.character(da$excluded$study)
    da            <- da$data
    recordColumn  <- as.numeric( c((default.val("num.charColumns")+1):dim(da)[2]) )
    studies       <- names(da)[recordColumn]
    
    if(length(da) > default.val("num.charColumns")) {

      if (missing(perm)) {
        perm 	<- matrix(studies[permutations(length(studies))], ncol=length(studies)) 
        message(paste("All possible permutations (", dim(perm)[1], ") have been calculated, now merging.", sep = ""))
      }
      
      
      # compute the mean correlation between merged responses and original response.
      meanCorrel <- matrix(NA,nrow=dim(perm)[1])
      
      for (i in 1:dim(perm)[1]) {
        merge.try <- try(modelRPSEQ(data=da, SEQ=perm[i,], overlapValues = overlapValues, plot = plot),silent = TRUE)
        if (inherits(merge.try, "try-error")) { 
          meanCorrel_merge.try <- NA 
        } else {
          meanCorrel_merge.try <- mean(unlist(sapply(da[,recordColumn],function(x) calModel(merge.try, x)[[1]]["MD"])))
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
      merge <- modelRPSEQ(data = da, SEQ = perm, overlapValues = overlapValues, plot = plot)
    } else {
      merge <- rep(NA, dim(da)[1])
      message("No data left to merge, returning NAs")
    }
    
  } else { # END if (permutation == TRUE)
    merge <- modelRP(da, glob.normalization = FALSE, select.MDValue = select.MDValue, overlapValues = overlapValues, plot = plot)
    excluded <- merge$excluded.data
    merge <- merge$model.response[,"merged_data"]
  }
  
  # update  response.matrix_non.normalized
  merged_data_withInChIKey <- data.frame(InChIKey = da$InChIKey, merged_data = merge)
  matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(responseMatrix_nn))
  findNA_InChIKey <- which(is.na(matchInChIKey))
  if (!is.na(findNA_InChIKey[1])) {
    addRow <- matrix(NA, nrow=length(findNA_InChIKey), ncol= dim(responseMatrix_nn)[2])
    colnames(addRow) <- colnames(responseMatrix_nn)
    rownames(addRow) <- merged_data_withInChIKey[findNA_InChIKey,"InChIKey"]
    responseMatrix_nn <- rbind(responseMatrix_nn,addRow)
    responseMatrix <- rbind(responseMatrix,addRow)
    matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(responseMatrix_nn))
  }
  responseMatrix_nn[matchInChIKey, receptor] <- merge
  assign("response.matrix_non.normalized", responseMatrix_nn, envir = .GlobalEnv)
  message(paste("response.matrix_non.normalized has been updated for",receptor))
  
  # update response.matrix
  name.Stud    <- colnames(da)[recordColumn]
  mp_orx       <- match(colnames(da)[recordColumn], responseRange[,"study"])
  Rmax         <- apply(as.matrix(da[,recordColumn]),2,function(x) max(range(x,na.rm=TRUE)))
  Smax         <- responseRange[mp_orx,"max"]
  merged_data  <- globalNorm(RMAX = Rmax,SMAX = Smax, MV = merge, name.Stud = name.Stud, responseRange = responseRange, weightGlobNorm = weightGlobNorm)
  merged_data_withInChIKey <- data.frame(InChIKey = da$InChIKey, merged_data = merged_data)
  matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(responseMatrix))
  responseMatrix[matchInChIKey, receptor] <- merged_data
  
  assign("response.matrix", responseMatrix, envir = .GlobalEnv)
  message(paste("response.matrix has been updated for",receptor))
  
  # update response.matrix.excluded
  if (length(excluded) > 0) {
    excluded.data$excluded <- as.character(excluded.data$excluded)
    excluded.data[excluded.data$OR == receptor, "excluded"] <- paste(excluded, collapse = ", ")
    assign("excluded.data", excluded.data, envir = .GlobalEnv)
    message(paste("excluded.data has been updated for",receptor))
  }
  
}
