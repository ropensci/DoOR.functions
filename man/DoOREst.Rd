\name{DoOREst}
\Rdversion{1.1}
\alias{DoOREst}
\title{
Estimate the missing entries in a response data
}
\description{
Estimate the missing entries in a response data
}
\usage{
DoOREst(da, nodor, method = "PC")
}
\arguments{
  \item{da}{
a data frame or matrix; contaning the consensus response values.
}
  \item{nodor}{
a numeric value; specifying the number of the selected odors.
}
  \item{method}{
character string; specifying the method ("PC" (Pearson's coefficient) and "Knn" (k nearest neighbors)) for estimation, the default is "PC".
}
}
\details{
A warp programe using \code{\link{LLSIestPC}} or \code{\link{LLSIestKnn}} estimate the missing entries in a response data.
}
\references{
Kim, H.; Golub, G. H. & Park, H., Missing value estimation for DNA microarray gene expression data: local least squares imputation., 2005, Bioinformatics, 21, 187-198
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
library(DoOR.data)
data(response.matrix)
# est_data <- DoOREst(da=response.matrix, nodor = 3)
}
\keyword{ math }
