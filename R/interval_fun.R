# intrval function
# 
# intrval function with a specified middle function.
# 
# The whole range for the interval function is [0, 1]. The \code{range.X} is a
# interval for applying the specified middle function \code{middleFun}, and
# the parameters for this middle function are given in \code{parms}. A liear
# function with slope 1 will be applied in the interval parts [0, range.X[1]]
# and [range.X[2], 1] and connect with middle function to form a closed model
# function.
# 
# @param x numeric vector of data values
# @param range.X numeric vector, specifying the range of x-axis for applying
# middle function.
# @param middleFun a function
# @param parms numeric vector, parameters of middle function
# @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
# @keywords math
#
# 
# 
# 
interval_fun <-
  function(x, range.X, middleFun, parms) {
    # if the values of input are small than the minimum of range.X, apply lowerFun, a linear function with a slope that equal 1.
    lowerFun <- function(x, parms) {
      expr1 <- middleFun(range.X[1], parms)
      return((expr1 - range.X[1]) + x)
    }
    
    # if the values of input are bigger than the maximum of range.X, apply upperFun, a linear function with a slope that equal 1.
    upperFun <- function(x,parms) {
      expr1 <- middleFun(range.X[2],parms) 		
      return((expr1 - range.X[2]) + x)
    }
    
    if (is.na(x[1])) { 
      y = NA 
    } else {
      if (x[1] > (range.X[1]) & x[1] < (range.X[2])) { y = middleFun(x, parms) }
      
      if (x[1] <= (range.X[1])) { y = lowerFun(x,parms) }
      
      if (x[1] >= (range.X[2])) { y = upperFun(x,parms) }
    }
    return(as.vector(y))
  }
