\name{inverse_modelfunction_asymp}
\Rdversion{1.1}
\alias{inverse_modelfunction_asymp}
\title{
inverse Asymptotic Model
}
\description{
inverse Asymptotic Model
}
\usage{
inverse_modelfunction_asymp(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data values
}
  \item{parms}{
numeric vector; parameters with given names: "Asym", "R0", "lrc"
}
}
\details{
expression : -log((input-Asym)/(R0-Asym))/exp(lrc)
}
\references{
Jose Pinheiro and Douglas Bates
}
\examples{
x <- seq(0.1,1,length=20)
y <- inverse_modelfunction_asymp(input = x, parms = c(Asym = 2, R0 = 0.2, lrc = 0.3) )
plot(x,y)
}
\keyword{ math }
