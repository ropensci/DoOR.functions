\name{modelRPSEQ}
\Rdversion{1.1}
\alias{modelRPSEQ}
\title{
Generates a model response and merge data in given sequence
}
\description{
Generates a model response and merge data in given sequence.
}
\usage{
modelRPSEQ(data, SEQ)
}
\arguments{
  \item{data}{
data frame; odorant response data, e.g. Or22a.
}
  \item{SEQ}{
character vector; containing the names of studies indicating given sequence for merging data.
}
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
library(DoOR.data)
data(Or35a)
data(response.range)
SEQ <- c("Hallem.2006.EN","Kreher.2008.EN","Yao.2005.WT")
selected.merg <- modelRPSEQ(Or35a, SEQ = SEQ)
}
\keyword{ data }
