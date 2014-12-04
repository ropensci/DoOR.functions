dsdx_sigmoid <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on Logistic Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "xmid", "scal"

## expression : sqrt(1+(Asym * ((exp((xmid - input)/scal)) * (1/scal))/(1 + (exp((xmid - input)/scal)))^2)^2)

{
	.value <- sqrt(1+(parms["Asym"] * ((exp((parms["xmid"] - input)/parms["scal"])) * (1/parms["scal"]))/(1 + (exp((parms["xmid"] - input)/parms["scal"])))^2)^2)

	.value
}
