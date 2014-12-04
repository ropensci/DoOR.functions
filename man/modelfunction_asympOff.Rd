\name{modelfunction_asympOff}
\Rdversion{1.1}
\alias{modelfunction_asympOff}
\title{
Asymptotic Model with an Offset
}
\description{
Asymptotic Model with an Offset
}
\usage{
modelfunction_asympOff(input, parms)
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
expression : Asym*(1 - exp(-exp(lrc)*(input - c0)))
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSasympOff}}
}
\examples{
x <- rnorm(20)
y <- modelfunction_asympOff(input = x, parms = c(Asym = 0.1, lrc = 0.2, c0 = 0.3) )
plot(x,y)
}
\keyword{ math }