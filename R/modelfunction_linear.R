# Linear Model
# 
# Linear Model
# 
# expression : Intercept + Slope * input
# 
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Intercept",
# "Slope"
# @keywords math
# @examples
# 
# x <- rnorm(20)
# y <- modelfunction_linear(input = x, parms = c(Intercept = 0.1, Slope = 0.2) )
# plot(x,y)
# 
modelfunction_linear <-
function(input, parms) 

## Linear Model

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# input : numeric vector of data values
# parms : numeric vector, parameters with given names: "Intercept", "Slope"

## expression : Intercept + Slope * input

{ 
    return(parms['Intercept'] + parms['Slope'] * input )
}
