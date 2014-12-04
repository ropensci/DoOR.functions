\name{inverse_modelfunction_linear}
\Rdversion{1.1}
\alias{inverse_modelfunction_linear}
\title{
inverse Linear Model
}
\description{
inverse Linear Model
}
\usage{
inverse_modelfunction_linear(input, parms)
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
expression : (input - Intercept) / Slope
}
\examples{
x <- seq(0.1,1,length=20)
y <- inverse_modelfunction_linear(input = x, parms = c(Intercept = 0.1, Slope = 0.2) )
plot(x,y)
}
\keyword{ math }
