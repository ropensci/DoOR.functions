#' model_response_seq
#'
#' generates a model response and merge data in given sequence
#'
#'
#' @param data data frame, odorant response data, e.g. Or22a.
#' @param SEQ character vector, containing the names of studies indicating given
#'   sequence for merging data.
#' @param overlapValues minimum overlap between studies to perfom a merge
#' @param select.MDValue the minimum mean distance between studies to perfom a
#'   merge
#' @param plot logical
#' @param strict logical, if TRUE merging a permutation will be stopped once a
#'   single merge has a mean distance above select.MDValue
#'
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @export
#' @importFrom stats na.omit
#' @importFrom graphics par points
#' @details # model_response_seq.R: #################
#'
#' # merges studies in a given sequence (determined by the user or by exhaustive
#' enumeration and choosing the optimal sequence)
#'
#'
#' # input parameters: ####################
#'
#'
#' # data  : data frame, odorant response data for a given receptor, e.g. Or22a
#' # SEQ 	: character vector, contains the names of studies that measured this
#' receptor in a specific order (the merging sequence)
#'
#' # output is a numeric vector: response values
#' @aliases modelRPSEQ model_response_seq
#'
#' @examples
#' # load data
#' library(DoOR.data)
#' data(Or35a)
#' data(door_response_range)
#' 
#' # specify a sequence of merging
#' SEQ <- c("Hallem.2006.EN","Kreher.2008.EN","Hallem.2006.EN")
#' 
#' # perform the merging
#' selected_merg <- model_response_seq(Or35a, SEQ = SEQ, plot = TRUE)
#'
model_response_seq <-
  function(data,
           SEQ,
           overlapValues  = door_default_values("overlapValues"),
           select.MDValue = door_default_values("select.MDValue"),
           strict         = TRUE,
           plot           = FALSE) {
    nv          <-
      as.numeric(c((
        door_default_values("num.charColumns") + 1
      ):dim(data)[2])) # positions of columns that contain odor response vectors
    name.stud <- names(data)[nv]
    pda <- apply(as.data.frame(data[, nv]), 2, door_norm) # processing data
    # match given sequence to the column names of response data
    mseq <- match(SEQ, name.stud) 
    first.study <- SEQ[1]
    rest.study  <- SEQ[-match(first.study, SEQ)]
    
    if (plot == TRUE)  {
      lenNV_1  <- length(nv) - 1
      nframe.X <- ceiling(lenNV_1 ^ 0.5)
      nframe.Y <- ceiling(lenNV_1 / nframe.X)
      op       <- par(mfrow = c((nframe.Y), (nframe.X)))
    }
    
    # start merging following the given sequence
    y <- pda[, first.study]
    ylab <- first.study
    for (i in rest.study) {
      x 	  <- pda[, i]
      if (dim(na.omit(cbind(x, y)))[1] < overlapValues)
        stop(paste(
          "less than",
          overlapValues,
          "observations between two datasets"
        ))
      
      projected <-
        project_points(
          x,
          y,
          plot = plot,
          xlab = i,
          ylab = ylab,
          title = plot
        )
      
      if (strict == TRUE & projected$MD > select.MDValue)
        stop(paste("Mean distance between two studies above", select.MDValue))
      
      res 	  <- rep(NA, length = dim(pda)[1])
      
      # the output of project_points is a list with odor responses, either
      # observed in both studies, or only in one study.
      res[projected$double.observations$ID] <-
        projected$double.observations$NDR
      if (is.data.frame(projected$single.observations)) {
        res[projected$single.observations$ID] <-
          projected$single.observations$NDR
      }
      
      y <- res
      ylab <- "merged_data"
      
    } # END merging
    
    return(y)
  }
