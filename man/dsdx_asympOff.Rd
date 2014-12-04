\name{dsdx_asympOff}
\Rdversion{1.1}
\alias{dsdx_asympOff}
\title{
Leibniz's notation for computing the curvic length on Asymptotic Model with an Offset
}
\description{
Leibniz's notation for computing the curvic length on Asymptotic Model with an Offset
}
\usage{
dsdx_asympOff(input, parms)
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
expression : sqrt(1+(Asym * (exp(-exp(lrc) * (input - c0)) * exp(lrc)))^2)
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSasympOff}}
}
\examples{
x <-  seq(0.1,1,length=20)
parms <- c(Asym = 0.2, lrc = 0.3, c0 = 0.4)
integrate(function(x) { dsdx_asympOff(input = x, parms = parms ) }, lower = 0, upper = 0.5)
}
\keyword{ math }
