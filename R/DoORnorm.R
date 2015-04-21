#' normalize the data in values from 0 to 1
#' 
#' \code{DoORnorm} is used to normalize the data in values from 0 to 1.
#' 
#' 
#' @param x a numeric vector
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords math
#' @examples
#' 
#' x <- rnorm(10)
#' DoORnorm(x)
#' 
DoORnorm <-
function(x) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## normalize the data in values from 0 to 1

# x: numeric vector

{
        if ((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) == 0) {
        	eval <- x - x
	}
        else { 
		eval <- (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) 
	}
        return(eval)
}
