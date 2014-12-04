\name{loadRD}
\Rdversion{1.1}
\alias{loadRD}
\title{
Load Data
}
\description{
Load all datasets including the precomputed response model.
}
\usage{
loadRD(odorantReceptors = default.val("ORs"), supportData = TRUE)
}
\arguments{
  \item{odorantReceptors}{
data frame; containing receptor or ORN names and their expression.
}
  \item{supportData}{
logical; if TRUE, load support data.
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
}
\keyword{ data }
