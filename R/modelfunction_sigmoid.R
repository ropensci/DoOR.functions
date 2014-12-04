modelfunction_sigmoid <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Logistic Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "xmid", "scal"

## expression : Asym/(1+exp((xmid-input)/scal))
## reference and author(s) : Jose Pinheiro and Douglas Bates

{
	return(SSlogis(input, Asym = parms["Asym"], xmid = parms["xmid"], scal = parms["scal"]))
}
