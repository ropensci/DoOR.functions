# Leibniz's notation for computing the curvic length on inverse exponential
# model
#
# Leibniz's notation for computing the curvic length on inverse exponential
# model
#
# expression : sqrt(1 + (1/b/((input - a)/b)/c)^2)
#
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "a", "b", "c"
# @seealso \code{\link{SSexpo}}
# @references Jose Pinheiro and Douglas Bates
# @keywords math
# @examples
#
# x <-  seq(0.1,1,length=20)
# parms <- c(a = 0, b = 0.2, c = 0.3)
# integrate(function(x) { dsdx_exp_inverse(input = x, parms = parms ) }, 
# lower = 0.1, upper = 0.5)
#
#' @importFrom stats integrate
dsdx_exp_inverse <- function(input, parms) {
  .value <-
    sqrt(1 + (1 / parms["b"] / ((input - parms["a"]) / parms["b"]) / 
                parms["c"]) ^ 2)
  .value
}
