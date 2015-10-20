# Leibniz's notation for computing the curvic length on Asymptotic Model with
# an Offset
# 
# Leibniz's notation for computing the curvic length on Asymptotic Model with
# an Offset
# 
# expression : sqrt(1+(Asym * (exp(-exp(lrc) * (input - c0)) * exp(lrc)))^2)
# 
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Asym", "lrc",
# "c0"
# @seealso \code{\link{SSasympOff}}
# @references Jose Pinheiro and Douglas Bates
# @keywords math
# @examples
# 
# x <-  seq(0.1,1,length=20)
# parms <- c(Asym = 0.2, lrc = 0.3, c0 = 0.4)
# integrate(function(x) { dsdx_asympOff(input = x, parms = parms ) }, lower = 0, upper = 0.5)
# 
dsdx_asympOff <- function(input, parms) {
  .value <- sqrt(1+(parms["Asym"] * (exp(-exp(parms["lrc"]) * (input - parms["c0"])) * exp(parms["lrc"])))^2)
  .value
}
