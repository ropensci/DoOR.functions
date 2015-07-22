#' compute the data pairwise and and selects a pair with the lowest "MD" value.
#'
#' compute the data pairwise using function \code{\link{calModel}} and selects
#' a pair with the lowest "MD" value.
#'
#' This function is used in \code{\link{modelRP}} to select the first pair or
#' next data set for merging. The output is a list containing "selected.x" and
#' "selected.y" that specify which data plots against another, and "best.model"
#' that is used in function \code{\link{projectPoints}}.
#'
#' @param candidate a character vector; contains the names of studies.
#' @param data_candidate a data frame; odorant response data that only contains
#' value columns.
#' @param merged_data numeric vector; merged data
#' @param overlapValues numeric; a criterion using to refuse a data set that
#' has not enough overlap value.
#' @param merged logical; if merged is \code{TRUE}, calculate models between
#' merged_data and candidate data. If \code{FALSE}, calculate models between
#' candidates.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @seealso \code{\link{projectPoints}},\code{\link{modelRP}}
#' @keywords data
#' @export
#' @examples
#'
#' library(DoOR.data)
#' data(ac3B)
#' studies<-names(ac3B)[c(7:9)]
#' data_candidate<-ac3B[,c(7:9)]
#' Norm_data_candidate <-apply(data_candidate, 2, DoORnorm)
#' selectModel(candidate = studies, data_candidate = Norm_data_candidate, merged = FALSE)
#'
selectModel <- function(candidate,
                        data_candidate,
                        merged_data,
                        overlapValues = default.val("overlapValues"),
                        merged = default.val("merged") ) {
  initial <- list(
    model.name = "no.fitted.model", cal.parameters = NA, MD = default.val("select.MDValue")
  )
  curr.model    <- list(initial = initial)
  best.model    <- curr.model
  selected.y    <- character()
  selected.x    <- character()
  len.cand 	    <- length(candidate)
  nam.data.cand <- colnames(data_candidate)
  seq.cand 	    <- match(candidate,nam.data.cand)
  
  
  if (merged == TRUE) {
    seq.i <- 1
  } else {
    seq.i <- 1:(len.cand - 1)
  }
  
  for (i in seq.i) {
    if (merged == TRUE) {
      y <- merged_data
      i <- 0
    } else {
      y <- data_candidate[,i]
    }
    
    for (j in (i + 1):len.cand) {
      # different data sets - map them
      x   <- data_candidate[,seq.cand[j]]
      x_y <- na.omit(cbind(x,y))
      
      # skip if
      # if there are no overlapping data points (i.e. disjunct odor sets)
      # or if there are less than "overlapValues" (too few to map)
      
      if (dim(x_y)[1] < overlapValues) {
        next
      }
      
      # skip also if
      # slope between x and y is 0 (horizontal line) or NA (vertical line).
      if (is.na(lm(y ~ x)$coef[2]) | lm(y ~ x)$coef[2] == 0) {
        next
      }
      curr.model <-
        suppressWarnings(calModel(
          x = x, y = y, select.MD = TRUE
        ))
      
      # skip if no fit could be performed
      if(is.na(curr.model))
        next
      
      # update best.model if a better fitting was found
      if (curr.model[[1]]$MD < best.model[[1]]$MD) {
        best.model <- curr.model
        selected.x <- candidate[j]
        if (merged == FALSE) {
          selected.y <- candidate[i]
        } else {
          selected.y <- "merged_data"
        }
      }
      
    } # END for (j in (i+1):len.cand)
  } # END for (i in seq.i)
  
  
  return(list(
    best.model = best.model, selected.x = selected.x, selected.y = selected.y
  ))
}
