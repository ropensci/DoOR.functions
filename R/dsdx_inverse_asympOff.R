#' Leibniz's notation for computing the curvic length on inverse Asymptotic
#' Model with an Offse
#' 
#' Leibniz's notation for computing the curvic length on inverse Asymptotic
#' Model with an Offse
#' 
#' expression : sqrt(1+(1/Asym/(1 - input/Asym)/(exp(lrc)))^2)
#' 
#' @param input numeric vector of data values
#' @param parms numeric vector; parameters with given names: "Asym", "lrc",
#' "c0"
#' @seealso \code{\link{SSasympOff}}
#' @references Jose Pinheiro and Douglas Bates
#' @keywords math
#' @examples
#' 
#' x <-  seq(0.1,1,length=20)
#' parms <- c(Asym = 2, R0 = 0.2, lrc = 0.3)
#' integrate(function(x) { dsdx_inverse_asympOff(input = x, parms = parms ) }, lower = 0, upper = 0.5)
#' 
dsdx_inverse_asympOff <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on inverse Asymptotic Model with an Offset

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "lrc", "c0"

## expression : sqrt(1+(1/Asym/(1 - input/Asym)/(exp(lrc)))^2)

{
	.value <- sqrt(1+(1/parms["Asym"]/(1 - input/parms["Asym"])/(exp(parms["lrc"])))^2)
	.value
}
