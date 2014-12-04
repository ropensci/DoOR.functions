\name{dsdx_inverse_asympOff}
\Rdversion{1.1}
\alias{dsdx_inverse_asympOff}
\title{
Leibniz's notation for computing the curvic length on inverse Asymptotic Model with an Offse
}
\description{
Leibniz's notation for computing the curvic length on inverse Asymptotic Model with an Offse
}
\usage{
dsdx_inverse_asympOff(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data values
}
  \item{parms}{
numeric vector; parameters with given names: "Asym", "lrc", "c0"
}
}
\details{
expression : sqrt(1+(1/Asym/(1 - input/Asym)/(exp(lrc)))^2)
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSasympOff}}
}
\examples{
x <-  seq(0.1,1,length=20)
parms <- c(Asym = 2, R0 = 0.2, lrc = 0.3)
integrate(function(x) { dsdx_inverse_asympOff(input = x, parms = parms ) }, lower = 0, upper = 0.5)
}
\keyword{ math }
