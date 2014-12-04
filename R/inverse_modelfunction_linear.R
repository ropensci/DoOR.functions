inverse_modelfunction_linear <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## inverse Linear Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Intercept", "Slope"

## expression : (input - Intercept) / Slope

{
	return((input - parms['Intercept']) / parms['Slope'])
}
