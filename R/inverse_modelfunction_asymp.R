inverse_modelfunction_asymp <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## inverse Asymptotic Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "R0", "lrc"

## expression : -log((input-Asym)/(R0-Asym))/exp(lrc)
## reference and author(s) : Jose Pinheiro and Douglas Bates

{
	subfun <- function(input) 
	{
		exp1  <- (input-parms["Asym"])/(parms["R0"]-parms["Asym"])
		if (exp1[1] <= 0 | is.na(exp1[1]))
		{ 
			return(NA)
		}
		else 
		{
			return(-log((input-parms["Asym"])/(parms["R0"]-parms["Asym"]))/exp(parms["lrc"]))
		}
	}
	return(as.vector(sapply(input, subfun)))
}
