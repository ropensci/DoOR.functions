\name{PseudoInverse}
\Rdversion{1.1}
\alias{PseudoInverse}
\title{
Evaluate the pseudoinvese of a matrix
}
\description{
Evaluate the pseudoinvese of a matrix
}
\usage{
PseudoInverse(x)
}
\arguments{
  \item{x}{
a matrix
}
}
\details{
The pseudoinverse of the matrix x with singular value decomposition \eqn{x=U D V^'} is:
\eqn{x^+ = V D^+ U^'}
where D^+ is the pseudoinverse of D, which is formed by replacing every nonzero entry by its reciprocal.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\seealso{
\code{\link{LLSIestPC}}, \code{\link{svd}}
}
\examples{
x  <- matrix(seq(1,2,length=4),nrow = 2, ncol = 2)
PI <- PseudoInverse(x)
x
x%*%PI%*%x
}
\keyword{ math }
