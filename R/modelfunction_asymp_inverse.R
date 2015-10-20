# inverse Asymptotic Model
# 
# inverse Asymptotic Model
# 
# expression : -log((input-Asym)/(R0-Asym))/exp(lrc)
# 
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Asym", "R0",
# "lrc"
# @references Jose Pinheiro and Douglas Bates
# @keywords math
# @examples
# 
# x <- seq(0.1,1,length=20)
# y <- modelfunction_asymp_inverse(input = x, parms = c(Asym = 2, R0 = 0.2, lrc = 0.3) )
# plot(x,y)
# 
modelfunction_asymp_inverse <- function(input, parms) {
  subfun <- function(input) 
  {
    exp1  <- (input-parms["Asym"])/(parms["R0"]-parms["Asym"])
    if (exp1[1] <= 0 | is.na(exp1[1])) { 
      return(NA)
    } else {
      return(-log((input-parms["Asym"])/(parms["R0"]-parms["Asym"]))/exp(parms["lrc"]))
    }
  }
  return(as.vector(sapply(input, subfun)))
}
