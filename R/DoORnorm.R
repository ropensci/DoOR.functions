#' rescale the data values from 0 to 1
#' 
#' \code{DoORnorm} is used to normalize the data in values from 0 to 1.
#' 
#' 
#' @param x a numeric vector
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords math
#' @examples
#' 
#' x <- rnorm(10)
#' DoORnorm(x)
#' 
DoORnorm <- function(x) {
  x.max <- max(x, na.rm = TRUE)
  x.min <- min(x, na.rm = TRUE)
  if (xmax - xmin) == 0) {
    eval <- x - x
	} else { 
		  eval <- (x - x.min) / (x.max - x.min) 
	}
return(eval)
}
