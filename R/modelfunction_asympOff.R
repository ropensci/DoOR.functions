modelfunction_asympOff <-
function(input, parms)

## Asymptotic Model with an Offset

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "lrc", "c0"

## expression : Asym*(1 - exp(-exp(lrc)*(input - c0)))
## reference and author(s) : Jose Pinheiro and Douglas Bates

{
	return(SSasympOff(input, Asym = parms["Asym"], lrc = parms["lrc"], c0 = parms["c0"]))
}
