# inverse Asymptotic Model with an Offset
#
# inverse Asymptotic Model with an Offset
#
# expression : c0 - (log(1 - (input / Asym)) / exp(lrc))
#
# @param input numeric vector of data values
# @param parms numeric vector, parameters with given names: "Asym", "c0",
# "lrc"
# @seealso \code{\link{SSasympOff}}
# @references Jose Pinheiro and Douglas Bates
# @keywords math
# @examples
#
# x <- seq(0.1,1,length=20)
# y <- modelfunction_asymp_off_inverse(input = x, parms = c(Asym = 2, c0 = 0.2, lrc = 0.3) )
# plot(x,y)
#
modelfunction_asymp_off_inverse <- function(input, parms) {
  subfun <- function(input) {
    exp1  <- 1 - (input / parms["Asym"])
    if (exp1[1] <= 0 | is.na(exp1[1])) {
      return(NA)
    } else {
      return(parms["c0"] - (log(1 - (input / parms["Asym"])) /
                              exp(parms["lrc"])))
    }
  }
  return(as.vector(sapply(input, subfun)))
}
