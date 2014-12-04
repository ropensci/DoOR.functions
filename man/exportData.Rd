\name{exportData}
\Rdversion{1.1}
\alias{exportData}
\title{
export data
}
\description{
export odor response data and supported data
}
\usage{
exportData(file.format, directory, odorantReceptors = default.val("ORs"), responseMatrix = default.val("response.matrix"), responseRange = default.val("response.range"), unglobalNorm_RM = default.val("unglobalNorm_response.matrix"), weightGlobNorm = default.val("weight.globNorm"), all.data = TRUE)
}
\arguments{
  \item{file.format}{
character string; the format of given file, either ".txt" or ".csv"
}
  \item{directory}{
character string; naming a directory for writing. If missing, the exported data are saved in current working directory.
}
  \item{odorantReceptors}{
data frame; receptor names and expressions
}
  \item{responseMatrix}{
data matrix; an global unnormalized responses matrix
}
  \item{responseRange}{
data frame; response ranges for each study
}
  \item{unglobalNorm_RM}{
data matrix; an unnormalized responses matrix
}
  \item{weightGlobNorm}{
data frame; weight matrix for global normalizazion
}
  \item{all.data}{
logical; if TRUE, export odorant response data and supported data "response.matrix", "response.range", "unglobalNorm_response.matrix", "response.matrix", "weight.globNorm" and "ORs".
}
}
\details{
Please load ORs from data package DoOR.data by typing (\code{data(ORs)}) before use.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
library(DoOR.data)
library(DoOR.function)
loadRD()
exportData(".txt", all.data = FALSE) 	# export odorant responses data only
}
\keyword{ data }
