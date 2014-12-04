\name{inverse_modelfunction_sigmoid}
\Rdversion{1.1}
\alias{inverse_modelfunction_sigmoid}
\title{
inverse Logistic Model
}
\description{
inverse Logistic Model
}
\usage{
inverse_modelfunction_sigmoid(input, parms)
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
expression : xmid - scal * log((Asym / input) - 1)
}
\references{
Jose Pinheiro and Douglas Bates
}
\seealso{
\code{\link{SSlogis}}
}
\examples{
x <- seq(0.1,1,length=20)
y <- inverse_modelfunction_sigmoid(input = x, parms = c(Asym = 2, xmid = 0.2, scal = 0.3) )
plot(x,y)
}
\keyword{ math }
