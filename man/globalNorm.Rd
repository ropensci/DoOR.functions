\name{globalNorm}
\Rdversion{1.1}
\alias{globalNorm}
\title{
normalize the merged data globally using weight
}
\description{
normalize the merged data globally using weight
}
\usage{
globalNorm(RMAX, SMAX, MV, name.Stud, weightGlobNorm = default.val("weight.globNorm"), responseRange = default.val("response.range") )
}
\arguments{
  \item{RMAX}{
numeric vector; a vector across all studies of maximum responses for this receptor.
}
  \item{SMAX}{
numeric vector; a vector across all studies of maximum response in all receptors available in each study.
}
  \item{MV}{
numeric vector; a vector of model response values to be normalized (for this receptor).
}
  \item{name.Stud}{
character vector; names of involved studies.
}
  \item{weightGlobNorm}{
data matrix; a receptor-by-study matrix; value 1 indicates available measurement in this study for this receptor; value NA indicates NOT.
}
  \item{responseRange}{
data frame; the response range and how many odors were measured in each study.
}
}
\details{
Some data contribute more than others based on how many receptors and odors were measured, so that we introduce a weighted mean to scale the model response values. If all weights are equal, then the weighted mean is the same as the arithmetic mean. expression for weighted scale:  ( sum(weight_receptors[i]*(RMAX[i]/SMAX[i])) + sum(weight_odors[i]*(RMAX[i]/SMAX[i])) ) / ( sum(weight_receptors[i]) + sum(weight_odors[i]) ). The global normalized output is equal to that merged consensus vector (in values [0, 1]) multiply by a weighted scale.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{

rm(list=ls())
library(DoOR.data)
data(weight.globNorm)
data(response.range)
mergedValues_ORx <- seq(0,1, length= 20) 		# merged response values of receptor (ORx).
name.Stud <- c("Hallem.2006.EN", "Pelz.2006.ALEC50")
RMAX <- c(100, 4)		# maximum responses of receptor (ORx) from study "Hallem.2006.EN" and "Pelz.2006.ALEC50" respectively.
SMAX <- c(294,6.9)	# maximum recordings of all receptors from study "Hallem.2006.EN" and "Pelz.2006.ALEC50" respectively.
GN_values_ORx <- globalNorm(RMAX, SMAX, MV = mergedValues_ORx, name.Stud, weightGlobNorm = weight.globNorm, responseRange = response.range)
}
\keyword{ math }
