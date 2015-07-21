# Leibniz's notation for computing the curvic length on inverse Asymptotic
# Model
# 
# Leibniz's notation for computing the curvic length on inverse Asymptotic
# Model
# 
# expression : sqrt(1+(-(1/(R0 - Asym)/(input - Asym)/(R0 -
# Asym)/exp(lrc)))^2)
# 
# @param input numeric vector of data values
# @param parms numeric vector; parameters with given names: "Asym", "lrc",
# "R0"
# @seealso \code{\link{SSasymp}}
# @references Jose Pinheiro and Douglas Bates
# @keywords math
# @examples
# 
# x <-  seq(0.1,1,length=20)
# parms <- c(Asym = 2, R0 = 0.2, lrc = 0.3)
# integrate(function(x) { dsdx_asymp_inverse(input = x, parms = parms ) }, lower = 0, upper = 0.5)
# 
dsdx_asymp_inverse <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on inverse Asymptotic Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "lrc", "R0"

## expression : sqrt(1+(-(1/(R0 - Asym)/(input - Asym)/(R0 - Asym)/exp(lrc)))^2)

{
	.value <- sqrt(1+(-(1/(parms["R0"] - parms["Asym"])/(input - parms["Asym"])/(parms["R0"] - parms["Asym"])/exp(parms["lrc"])))^2)
	.value
}
