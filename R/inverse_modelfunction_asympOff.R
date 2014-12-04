inverse_modelfunction_asympOff <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## inverse Asymptotic Model with an Offset

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "Asym", "lrc", "c0"

## expression : c0 - (log(1 - (input / Asym)) / exp(lrc))
## reference and author(s) : Jose Pinheiro and Douglas Bates

{
	subfun <- function(input) 
	{
		exp1  <- 1 - (input / parms["Asym"])
		if (exp1[1] <= 0 | is.na(exp1[1]))
		{ 
			return(NA)
		}
		else 
		{
			return(parms["c0"] - (log(1 - (input / parms["Asym"])) / exp(parms["lrc"])))
		}
	}
	return(as.vector(sapply(input, subfun)))	
}
