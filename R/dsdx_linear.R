# Leibniz's notation for computing the curvic length on linear model
#
# Leibniz's notation for computing the curvic length on linear model
#
# expression : sqrt(1+(Slope)^2)*input^0
#
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Intercept",
# "Slope"
# @keywords math
# @examples
#
# x <-  seq(0.1,1,length=20)
# parms <- c(Intercept = 2, Slope = 0.2)
# integrate(function(x) { dsdx_linear(input = x, parms = parms ) }, lower = 0.1, upper = 0.5)
#
#' @importFrom stats integrate
dsdx_linear <- function(input, parms) {
  .value <- sqrt(1+(parms['Slope'])^2)*input^0
  .value
}
