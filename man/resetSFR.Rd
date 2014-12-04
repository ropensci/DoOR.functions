\name{resetSFR}
\Rdversion{1.1}
\alias{resetSFR}
\title{
reset SFR
}
\description{
A function for reseting SFR to zero
}
\usage{
resetSFR(x, sfr)
}
\arguments{
  \item{x}{
a numeric vector of input values.
}
  \item{sfr}{
a numieric parameter; indicating spontaneous firing rate value.
}
}
\details{
The expression is \code{(x-sfr)*(max(x))/(max(x)-sfr)}. If the spontaneous firing rate is missing \code{(NA)}, the value of sfr is given 0.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
library(DoOR.data)
data(response.matrix)
x=response.matrix[,'Or22a']
y=resetSFR(x=x,sfr=x[1])

}
\keyword{ math }
