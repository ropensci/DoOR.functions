inverse_modelfunction_sigmoid <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## inverse Logistic Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "xmid", "scal"

## expression : xmid - scal * log((Asym / input) - 1)
## reference and author(s) : Jose Pinheiro and Douglas Bates

{
	subfun <- function(input) 
	{
		exp1  <- (parms["Asym"] / input) - 1
		if (exp1[1] <= 0 | is.na(exp1[1]))
		{ 
			return(NA)
		}
		else 
		{
			return(parms["xmid"] - parms["scal"] * log((parms["Asym"] / input) - 1))
		}
	}
		return(as.vector(sapply(input, subfun)))
}
