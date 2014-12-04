dsdx_asympOff <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on Asymptotic Model with an Offset

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "lrc", "c0"

## expression : sqrt(1+(Asym * (exp(-exp(lrc) * (input - c0)) * exp(lrc)))^2)

{
	.value <- sqrt(1+(parms["Asym"] * (exp(-exp(parms["lrc"]) * (input - parms["c0"])) * exp(parms["lrc"])))^2)
	.value
}
