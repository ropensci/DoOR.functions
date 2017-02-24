#' reset SFR
#'
#' A function for reseting SFR to zero
#'
#' Performs a simple subtraction of the SFR value.
#'
#' @param x numeric or DoOR response matrix, input values
#' @param sfr numeric or character, either a value to subtract if x is a vector or an InChIKey if x is a DoOR response matrix
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases resetSFR reset_sfr
#' @export
#' @examples
#'
#' library(DoOR.data)
#' data(door_response_matrix)
#' door_response_matrix_SFRreset <- reset_sfr(door_response_matrix, "SFR")
#'
reset_sfr <- function(x, sfr) {
  if(is.numeric(x)) {
    if(!is.numeric(sfr))
      stop("As x is a vector, sfr has to be a numeric value to subtract!")
    if (is.na(sfr)) sfr <- 0
    if (all(is.na(x))) return(x)
    reset <- (x - sfr)
  } else {
    if(!(is.character(sfr) | is.factor(sfr)))
      stop("As x is a full DoOR response matrix, sfr has to be the InChIKey of the odorant that should be subtracted")
    reset   <- as.data.frame(apply(x, 2, function(y) reset_sfr(y,y[sfr])))
  }
  return(reset)
}
