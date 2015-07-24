# Leibniz's notation for computing the curvic length on inverse Logistic Model
# 
# Leibniz's notation for computing the curvic length on inverse Logistic Model
# 
# expression : sqrt(1+(scal * (Asym/input^2/(Asym/input - 1)))^2)
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
# parms <- c(Asym = 2, xmid = 0.2, scal = 0.3)
# integrate(function(x) { dsdx_sigmoid_inverse(input = x, parms = parms ) }, lower = 0.1, upper = 0.5)
# 
dsdx_sigmoid_inverse <-
function(input, parms)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Leibniz's notation for computing the curvic length on inverse Logistic Model

# input : numeric vector of data values
# parms : numeric vector, parameters with given names: "Asym", "xmid", "scal"

## expression : sqrt(1+(scal * (Asym/input^2/(Asym/input - 1)))^2)

{
	.value <- sqrt(1+(parms["scal"] * (parms["Asym"]/input^2/(parms["Asym"]/input - 1)))^2)
	.value
}
