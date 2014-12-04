dsdx_asymp <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on Asymptotic Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "lrc", "R0"

## expression : sqrt(1+(-((R0 - Asym) * (exp(-exp(lrc) * input) * exp(lrc))))^2)

{
	.value <- sqrt(1+(-((parms["R0"] - parms["Asym"]) * (exp(-exp(parms["lrc"]) * input) * exp(parms["lrc"]))))^2)
	.value
}
