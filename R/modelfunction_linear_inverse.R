# inverse Linear Model
# 
# inverse Linear Model
# 
# expression : (input - Intercept) / Slope
# 
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Intercept",
# "Slope"
# @keywords math
# @examples
# 
# x <- seq(0.1,1,length=20)
# y <- modelfunction_linear_inverse(input = x, parms = c(Intercept = 0.1, Slope = 0.2) )
# plot(x,y)
# 
modelfunction_linear_inverse <- function(input, parms) {
  return((input - parms['Intercept']) / parms['Slope'])
}
