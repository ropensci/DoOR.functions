# inverse exponential Model
# 
# inverse exponential Model
# 
# expression : (log((input-a)/b))/c
# 
# @param input numeric vector of data value
# @param parms numeric vector, parameters with given names: "a", "b", "c"
# @seealso \code{\link{SSexpo}}
# @keywords math
# @examples
# 
# x <- seq(0.1,1,length=20)
# y <- modelfunction_exp_inverse(input = x, parms = c(a = 0, b = 0.2, c = 0.3) )
# plot(x,y)
# 
modelfunction_exp_inverse <- function(input, parms) {
  subfun <- function(input) {
    exp1  <- (input-parms["a"])/parms["b"]
    if (exp1[1] <= 0 | is.na(exp1[1])) { 
      return(NA)
    } else {
      return((log((input-parms["a"])/parms["b"]))/parms["c"])
    }
  }
  return(vapply(input, subfun, numeric(1)))
}
