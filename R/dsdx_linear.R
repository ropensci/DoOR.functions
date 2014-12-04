dsdx_linear <-
function(input, parms) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on linear model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Intercept", "Slope"

## expression : sqrt(1+(Slope)^2)*input^0

{ 
    .value <- sqrt(1+(parms['Slope'])^2)*input^0
    .value 
}
