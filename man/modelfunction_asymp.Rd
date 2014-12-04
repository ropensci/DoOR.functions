\name{modelfunction_asymp}
\Rdversion{1.1}
\alias{modelfunction_asymp}
\title{
Asymptotic Model
}
\description{
Asymptotic Model
}
\usage{
modelfunction_asymp(input, parms)
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
expression : Asym+(R0-Asym)*exp(-exp(lrc)*input)
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSasymp}}
}
\examples{
x <- rnorm(20)
y <- modelfunction_asymp(input = x, parms = c(Asym = 0.1, R0 = 0.2, lrc = 0.3) )
plot(x,y)
}
\keyword{ math }
