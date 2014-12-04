modelfunction_exp <-
function(input, parms)

## exponential Model

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "a", "b", "c"

## expression : a + b * exp (c * input)

{
	return(SSexpo(input, a = parms["a"], b = parms["b"], c = parms["c"]))
}
