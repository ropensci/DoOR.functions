\name{DoORnorm}
\Rdversion{1.1}
\alias{DoORnorm}
\title{
normalize the data in values from 0 to 1
}
\description{
\code{DoORnorm} is used to normalize the data in values from 0 to 1.
}
\usage{
DoORnorm(x)
}
\arguments{
  \item{x}{
a numeric vector
}
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
x <- rnorm(10)
DoORnorm(x)
}
\keyword{ math }
