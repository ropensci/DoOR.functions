\name{findResp}
\Rdversion{1.1}
\alias{findResp}
\title{
Find receptor responses
}
\description{
given a chemical, get original receptor responses from all studies in the database.
}
\usage{
findResp(CAS, responseRange = default.val("response.range"), Or.list)
}
\arguments{
  \item{CAS}{
character string; CAS number
}
  \item{responseRange}{
data frame; response ranges of studies
}
  \item{Or.list}{
a list contains reponse data of  all available receptors. It can be loaded using \code{\link{loadRDList}}.
}
}
\details{
output is a data frame containing response values of given odor across receptors from all available studies.
}
\examples{
library(DoOR.data)
loadRD()
Or.list <- loadRDList()
cas="105-87-3"
OriginalResponses <- findResp(CAS = cas, responseRange = response.range, Or.list = Or.list)
}
\keyword{ data }
