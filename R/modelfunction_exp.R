# exponential Model
# 
# exponential Model
# 
# expression : a + b * exp (c * input)
# 
# @param input numeric vector of data values
# @param parms numeric vector; parameters with given names: "a", "b", "c"
# @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
# @seealso \code{\link{SSexpo}}
# @keywords math
# @examples
# 
# x <- rnorm(20)
# y <- modelfunction_exp(input = x, parms = c(a = 0.1, b = 0.2, c = 0.3) )
# plot(x,y)
# 
modelfunction_exp <-
function(input, parms)

## exponential Model

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# input : numeric vector of data values
# parms : numeric vector; parameters with given names: "a", "b", "c"

## expression : a + b * exp (c * input)

{
	return(SSexpo(input, a = parms["a"], b = parms["b"], c = parms["c"]))
}
