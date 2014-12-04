\name{intervalFun}
\Rdversion{1.1}
\alias{intervalFun}
\title{
intrval function
}
\description{
intrval function with a specified middle function.
}
\usage{
intervalFun(x, range.X, middleFun, parms)
}
\arguments{
  \item{x}{
numeric vector of data values
}
  \item{range.X}{
numeric vector; specifying the range of x-axis for applying middle function.
}
  \item{middleFun}{
a function
}
  \item{parms}{
numeric vector; parameters of middle function
}
}
\details{
The whole range for the interval function is [0, 1]. The \code{range.X} is a interval for applying the specified middle function \code{middleFun}, and the parameters for this middle function are given in \code{parms}. A liear function with slope 1 will be applied in the interval parts [0, range.X[1]] and [range.X[2], 1] and connect with middle function to form a closed model function.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{

}
\keyword{ math }
