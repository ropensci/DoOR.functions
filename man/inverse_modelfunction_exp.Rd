\name{inverse_modelfunction_exp}
\Rdversion{1.1}
\alias{inverse_modelfunction_exp}
\title{
inverse exponential Model
}
\description{
inverse exponential Model
}
\usage{
inverse_modelfunction_exp(input, parms)
}
\arguments{
  \item{input}{
numeric vector of data value
}
  \item{parms}{
numeric vector; parameters with given names:  "a", "b", "c"
}
}
\details{
expression : (log((input-a)/b))/c
}
\seealso{
\code{\link{SSexpo}}
}
\examples{
x <- seq(0.1,1,length=20)
y <- inverse_modelfunction_exp(input = x, parms = c(a = 0, b = 0.2, c = 0.3) )
plot(x,y)
}
\keyword{ math }
