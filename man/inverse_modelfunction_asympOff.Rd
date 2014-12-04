\name{inverse_modelfunction_asympOff}
\Rdversion{1.1}
\alias{inverse_modelfunction_asympOff}
\title{
inverse Asymptotic Model with an Offset
}
\description{
inverse Asymptotic Model with an Offset
}
\usage{
inverse_modelfunction_asympOff(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data values
}
  \item{parms}{
numeric vector; parameters with given names: "Asym", "c0", "lrc"
}
}
\details{
expression : c0 - (log(1 - (input / Asym)) / exp(lrc))
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSasympOff}}
}
\examples{
x <- seq(0.1,1,length=20)
y <- inverse_modelfunction_asympOff(input = x, parms = c(Asym = 2, c0 = 0.2, lrc = 0.3) )
plot(x,y)
}
\keyword{ math }
