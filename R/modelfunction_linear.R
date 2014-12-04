modelfunction_linear <-
function(input, parms) 

## Linear Model

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Intercept", "Slope"

## expression : Intercept + Slope * input

{ 
    return(parms['Intercept'] + parms['Slope'] * input )
}
