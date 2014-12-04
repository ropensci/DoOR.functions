\name{SSexpo}
\Rdversion{1.1}
\alias{SSexpo}
\title{
SelfStart estimation of exponential function
}
\description{
selfStart model estimates the parameters (a,b,c) of expontenial model.
}
\usage{
SSexpo(x, a, b, c)
}
\arguments{
  \item{x}{
a numeric vector indicates input values in an equation.
}
  \item{a}{
a numieric parameter.
}
  \item{b}{
a numieric parameter.
}
  \item{c}{
a numieric parameter.
}
}
\details{
The expression of eponential function is \code{a+b*exp(c*x)}.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\seealso{
\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
x <- -(1:100)/10
y <- 1 + 10 * exp(x / 2) + rnorm(x)/10
pr<-nls(y~SSexpo(x,a,b,c))
}
\keyword{ math }
