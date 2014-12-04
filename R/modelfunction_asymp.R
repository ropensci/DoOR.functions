modelfunction_asymp <-
function(input, parms)

## Asymptotic Model

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "R0", "lrc"

## expression : Asym+(R0-Asym)*exp(-exp(lrc)*input)
## reference and author(s) : Jose Pinheiro and Douglas Bates

{
	return(SSasymp(input, Asym = parms["Asym"], R0 = parms["R0"], lrc = parms["lrc"]))
}
