inverse_modelfunction_exp <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## inverse exponential Model

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "a", "b", "c"

## expression : (log((input-a)/b))/c

{
	subfun <- function(input) 
	{
		exp1  <- (input-parms["a"])/parms["b"]
		if (exp1[1] <= 0 | is.na(exp1[1]))
		{ 
			return(NA)
		}
		else 
		{
			return((log((input-parms["a"])/parms["b"]))/parms["c"])
		}
	}
	return(as.vector(sapply(input, subfun)))
}
