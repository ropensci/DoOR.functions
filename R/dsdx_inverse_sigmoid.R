dsdx_inverse_sigmoid <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on inverse Logistic Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "xmid", "scal"

## expression : sqrt(1+(scal * (Asym/input^2/(Asym/input - 1)))^2)

{
	.value <- sqrt(1+(parms["scal"] * (parms["Asym"]/input^2/(parms["Asym"]/input - 1)))^2)
	.value
}
