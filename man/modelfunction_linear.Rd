\name{modelfunction_linear}
\Rdversion{1.1}
\alias{modelfunction_linear}
\title{
Linear Model
}
\description{
Linear Model
}
\usage{
modelfunction_linear(input, parms)
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
expression : Intercept + Slope * input
}
\examples{
x <- rnorm(20)
y <- modelfunction_linear(input = x, parms = c(Intercept = 0.1, Slope = 0.2) )
plot(x,y)
}
\keyword{ math }
