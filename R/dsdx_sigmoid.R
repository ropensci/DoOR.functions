# Leibniz's notation for computing the curvic length on Logistic Model
#
# Leibniz's notation for computing the curvic length on Logistic Model
#
# expression : sqrt(1+(Asym * ((exp((xmid - input)/scal)) * (1/scal))/(1 +
# (exp((xmid - input)/scal)))^2)^2)
#
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Asym", "xmid",
# "scal"
# @seealso \code{\link{SSlogis}}
# @references Jose Pinheiro and Douglas Bates
# @keywords math
# @examples
#
# x <-  seq(0.1,1,length=20)
# parms <- c(Asym = 0.2, xmid = 0.2, scal = 0.4)
# integrate(function(x) { dsdx_sigmoid(input = x, parms = parms ) }, lower = 0.1, upper = 0.5)
#
#' @importFrom stats integrate
dsdx_sigmoid <- function(input, parms) {
  .value <- sqrt(1+(parms["Asym"] * ((exp((parms["xmid"] - input)/parms["scal"])) * (1/parms["scal"]))/(1 + (exp((parms["xmid"] - input)/parms["scal"])))^2)^2)

  .value
}
