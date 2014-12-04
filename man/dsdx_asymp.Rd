\name{dsdx_asymp}
\Rdversion{1.1}
\alias{dsdx_asymp}
\title{
Leibniz's notation for computing the curvic length on Asymptotic Model
}
\description{
Leibniz's notation for computing the curvic length on Asymptotic Model
}
\usage{
dsdx_asymp(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data values
}
  \item{parms}{
numeric vector; parameters with given names: "Asym", "lrc", "R0"
}
}
\details{
expression : sqrt(1+(-((R0 - Asym) * (exp(-exp(lrc) * input) * exp(lrc))))^2)
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSasymp}}
}
\examples{
x <-  seq(0.1,1,length=20)
parms <- c(Asym = 0.1, lrc = 0.2, R0 = 0.3)
integrate(function(x) { dsdx_asymp(input = x, parms = parms ) }, lower = 0, upper = 0.5)
}
\keyword{ math }
