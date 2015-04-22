#' reset SFR
#' 
#' A function for reseting SFR to zero
#' 
#' Performs a simple subtraction of the SFR value.
#' 
#' @param x a numeric vector of input values.
#' @param sfr a numeric parameter, indicating spontaneous firing rate value.
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @keywords math
#' @examples
#' 
#' library(DoOR.data)
#' data(response.matrix)
#' response.matrix.SFRreset <- apply(response.matrix, 2, function(x) resetSFR(x,x['SFR']))
#' 
resetSFR <- function(x, sfr) {
	if (is.na(sfr)) sfr <- 0
  if (all(is.na(x))) return(x)
  rS <- (x - sfr)
	return(rS)
}