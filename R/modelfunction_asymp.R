#' Asymptotic Model
#' 
#' Asymptotic Model
#' 
#' expression : Asym+(R0-Asym)*exp(-exp(lrc)*input)
#' 
#' @param input numeric vector of data values
#' @param parms numeric vector; parameters with given names: "Asym", "R0",
#' "lrc"
#' @seealso \code{\link{SSasymp}}
#' @references Jose Pinheiro and Douglas Bates
#' @keywords math
#' @examples
#' 
#' x <- rnorm(20)
#' y <- modelfunction_asymp(input = x, parms = c(Asym = 0.1, R0 = 0.2, lrc = 0.3) )
#' plot(x,y)
#' 
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
