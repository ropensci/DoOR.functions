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
resetSFR <-
function(x, sfr) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## A function for reseting SFR to zero
#  x 	: a numeric vector of input values. 
#  sfr 	: a numieric parameter; indicating spontaneous firing rate value.

{
	if (is.na(sfr)) { sfr <- 0 }
	if ((max(x,na.rm=TRUE)-sfr) == 0) { rS = (x-sfr)*0 }
	else 
	{
		rS <- (x-sfr) * (max(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - sfr)
	}
	rx <- range(rS,na.rm=TRUE)
	if (rx[1] < (-1)) { rS=rS/abs(rx[1]) }
	if (rx[1] > (1) ) { rS=rS/abs(rx[2]) }
	return(rS)
}
