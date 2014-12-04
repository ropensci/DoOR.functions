\name{mapReceptor}
\alias{mapReceptor}
\title{
Mapping the given receptor into response matrix
}
\description{
Mapping the given receptor into response matrix by comparing their linear relationship (Pearson Correlation Coefficient). 
}
\usage{
mapReceptor(da, by.column, Receptors, ResponseMatrix)
}
\arguments{
  \item{da}{
data frame; odorant response data for a given receptor, e.g. Or22a. 
}
  \item{by.column}{
character string; specifying the column in "da" that contains the response values.
}
  \item{Receptors}{
character vector; containing the names of receptor that specify the consensus values in response matrix
}
  \item{ResponseMatrix}{
output is a numeric vector that contains the Pearson Correlation Coefficient between given data and selected consensus data in response matrix
}
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\keyword{ data }
