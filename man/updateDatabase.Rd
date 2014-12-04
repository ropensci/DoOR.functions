\name{updateDatabase}
\Rdversion{1.1}
\alias{updateDatabase}
\title{
 update response matrix 
}
\description{
  update the globally \code{response matrix} and the unglobally normalized response matrix \code{unglobalNorm_response.matrix} by introducing new consensus response data of given receptor.
}
\usage{
updateDatabase(receptor, permutation = TRUE, unglobalNorm_responseMatrix = default.val("unglobalNorm_response.matrix"), responseMatrix = default.val("response.matrix"), responseRange = default.val("response.range"), weightGlobNorm = default.val("weight.globNorm"))
}
\arguments{
  \item{receptor}{
character string; name of given odorant receptor.
}
  \item{permutation}{
logical; if TRUE, the sequence is chosen from permutation, if FALSE, sequence is chosen by the routine process.
}
  \item{unglobalNorm_responseMatrix}{
data frame; response data that has not been globally normalized.
}
  \item{responseMatrix}{
data frame; globally normalized response data.
}
  \item{responseRange}{
data frame; response range of studies.
}
  \item{weightGlobNorm}{
data frame; weight matrix for global normalization.
}
}
\details{
The merging sequence could be arranged by the routine process (using \code{\link{modelRP}} or taking the optimized sequence that is chosen from permutations. The mean correlation between merged responses and each original recording will be computed for each permutation, the optimozed sequence is with the highest correlation.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\seealso{
\code{\link{modelRP}},\code{\link{modelRPSEQ}}, \code{\link[gregmisc]{permutations}}
}
\examples{
library(DoOR.data)
loadRD()
# update the entry "Or67b" of data "response.matrix" and "unglobalNorm_response.matrix" with permutations.
# updateDatabase(receptor="Or67b", permutation = TRUE)
}
\keyword{ data }
