dsdx_inverse_asymp <-
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
