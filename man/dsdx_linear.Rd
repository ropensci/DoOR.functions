\name{dsdx_linear}
\Rdversion{1.1}
\alias{dsdx_linear}
\title{
Leibniz's notation for computing the curvic length on linear model
}
\description{
Leibniz's notation for computing the curvic length on linear model
}
\usage{
dsdx_linear(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data values
}
  \item{parms}{
numeric vector; parameters with given names: "Intercept", "Slope"
}
}
\details{
expression : sqrt(1+(Slope)^2)*input^0
}
\examples{
x <-  seq(0.1,1,length=20)
parms <- c(Intercept = 2, Slope = 0.2)
integrate(function(x) { dsdx_linear(input = x, parms = parms ) }, lower = 0.1, upper = 0.5)
}
\keyword{ math }