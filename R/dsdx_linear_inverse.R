# Leibniz's notation for computing the curvic length on inverse linear model
# 
# Leibniz's notation for computing the curvic length on inverse linear model
# 
# expression : sqrt(1+(1/Slope)^2)*input^0
# 
# @param input numeric vector of data values
# @param parms numeric vector; parameters with given names: "Intercept",
# "Slope"
# @keywords math
# @examples
# 
# x <-  seq(0.1,1,length=20)
# parms <- c(Intercept = 0.2, Slope = 0.3) 
# integrate(function(x) { dsdx_linear_inverse(input = x, parms = parms ) }, lower = 0.1, upper = 0.5)
# 
dsdx_linear_inverse <-
function(input, parms) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on inverse linear model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Intercept", "Slope"

## expression : sqrt(1+(1/Slope)^2)*input^0

{ 
    .value <- sqrt(1+(1/parms['Slope'])^2)*input^0
    .value 
}
