# Linear Model
# 
# Linear Model
# 
# expression : Intercept + Slope * input
# 
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Intercept",
# "Slope"
# @keywords math
# @examples
# 
# x <- rnorm(20)
# y <- modelfunction_linear(input = x, parms = c(Intercept = 0.1, Slope = 0.2) )
# plot(x,y)
# 
modelfunction_linear <- function(input, parms) { 
  return(parms['Intercept'] + parms['Slope'] * input )
}
