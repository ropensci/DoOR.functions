\name{modelfunction_sigmoid}
\Rdversion{1.1}
\alias{modelfunction_sigmoid}
\title{
Logistic Model
}
\description{
Logistic Model
}
\usage{
modelfunction_sigmoid(input, parms)
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
expression : Asym/(1+exp((xmid-input)/scal))
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSlogis}}
}
\examples{
x <- rnorm(20)
y <- modelfunction_sigmoid(input = x, parms = c(Asym = 0.1, xmid = 0.2, scal = 0.3) )
plot(x,y)
}
\keyword{ math }
