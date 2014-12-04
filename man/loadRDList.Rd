\name{loadRDList}
\Rdversion{1.1}
\alias{loadRDList}
\title{
load data into a list
}
\description{
load all available data into a list
}
\usage{
loadRDList(odorantReceptors = default.val("ORs"))
}
\arguments{
  \item{odorantReceptors }{
data frame; containing receptor or ORN names and their expression.
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
orlist <- loadRDList()
orlist$Or22a
}
\keyword{ data }
