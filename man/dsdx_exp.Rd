\name{dsdx_exp}
\Rdversion{1.1}
\alias{dsdx_exp}
\title{
Leibniz's notation for computing the curvic length on exponential model
}
\description{
Leibniz's notation for computing the curvic length on exponential model
}
\usage{
dsdx_exp(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data values
}
  \item{parms}{
numeric vector; parameters with given names: "a", "b", "c"
}
}
\details{
expression : sqrt(1 + (b * (exp(input * c) * c))^2)
}
\examples{
x <-  seq(0.1,1,length=20)
parms <- c(a = 0.2, b = 0.3, c = 0.4)
integrate(function(x) { dsdx_exp(input = x, parms = parms ) }, lower = 0, upper = 0.5)
}
\keyword{ math }
