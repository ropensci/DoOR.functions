#' rescale the data values from 0 to 1
#'
#' \code{door_norm} is used to normalize the data in values from 0 to 1.
#'
#'
#' @param x a numeric vector
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases DoORnorm door_norm
#' @keywords math
#' @export
#' @examples
#' x <- rnorm(10)
#' door_norm(x)
#'
door_norm <- function(x) {
  if(all(is.na(x)))
     return(x)
  x.max <- max(x, na.rm = TRUE)
  x.min <- min(x, na.rm = TRUE)
  if ((x.max - x.min) == 0) {
    eval <- x - x
  } else {
    eval <- (x - x.min) / (x.max - x.min)
  }
  return(eval)
}
