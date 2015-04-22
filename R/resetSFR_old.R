#' reset SFR
#' 
#' A function for reseting SFR to zero
#' 
#' The expression is \code{(x-sfr)*(max(x))/(max(x)-sfr)}. If the spontaneous
#' firing rate is missing \code{(NA)}, the value of sfr is given 0.
#' 
#' @param x a numeric vector of input values.
#' @param sfr a numieric parameter; indicating spontaneous firing rate value.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords math
#' @examples
#' 
#' library(DoOR.data)
#' data(response.matrix)
#' x=response.matrix[,'Or22a']
#' y=resetSFR(x=x,sfr=x[1])
#' 
#' 
resetSFR_old <- function(x, sfr) {
  x.max <- max(x, na.rm = TRUE)
	if (is.na(sfr)) sfr <- 0

  if ((x.max - sfr) == 0) {
    rS = (x - sfr) * 0
	} else {
		rS <- (x - sfr) * (x.max) / (x.max - sfr)
	}

  rx <- range(rS, na.rm = TRUE)
	if (rx[1] < (-1)) rS = rS / abs(rx[1])
	if (rx[1] > (1) ) rS = rS / abs(rx[2])
	return(rS)
}
