\name{dsdx_inverse_sigmoid}
\Rdversion{1.1}
\alias{dsdx_inverse_sigmoid}
\title{
Leibniz's notation for computing the curvic length on inverse Logistic Model
}
\description{
Leibniz's notation for computing the curvic length on inverse Logistic Model
}
\usage{
dsdx_inverse_sigmoid(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data values
}
  \item{parms}{
numeric vector; parameters with given names: "Asym", "xmid", "scal"
}
}
\details{
expression : sqrt(1+(scal * (Asym/input^2/(Asym/input - 1)))^2)
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSlogis}}
}
\examples{
x <-  seq(0.1,1,length=20)
parms <- c(Asym = 2, xmid = 0.2, scal = 0.3)
integrate(function(x) { dsdx_inverse_sigmoid(input = x, parms = parms ) }, lower = 0.1, upper = 0.5)
}
\keyword{ math }
