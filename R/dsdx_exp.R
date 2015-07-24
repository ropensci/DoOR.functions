# Leibniz's notation for computing the curvic length on exponential model
# 
# Leibniz's notation for computing the curvic length on exponential model
# 
# expression : sqrt(1 + (b * (exp(input * c) * c))^2)
# 
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "a", "b", "c"
# @keywords math
# @examples
# 
# x <-  seq(0.1,1,length=20)
# parms <- c(a = 0.2, b = 0.3, c = 0.4)
# integrate(function(x) { dsdx_exp(input = x, parms = parms ) }, lower = 0, upper = 0.5)
# 
dsdx_exp <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on exponential model

# input : numeric vector of data values
# parms : numeric vector, parameters with given names: "a", "b", "c"

## expression : sqrt(1 + (b * (exp(input * c) * c))^2)

{
	.value <- sqrt(1 + (parms["b"] * (exp(input * parms["c"]) * parms["c"]))^2)
	.value
}
