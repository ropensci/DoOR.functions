\name{findRespNorm}
\Rdversion{1.1}
\alias{findRespNorm}
\title{
Find normalised receptor responses
}
\description{
given a chemical, get normalised receptor responses from all studies in the database.
}
\usage{
findRespNorm(CAS, zero = default.val("zero"), responseMatrix = default.val("response.matrix"))
}
\arguments{
  \item{CAS}{
character vector; one or more odors.
}
  \item{zero}{
character string; The default is "SFR", i.e. the spontaneous firing rate.
}
  \item{responseMatrix}{
 a data frame; as e.g. "response.matrix" that is loaded by \code{\link{modelRP}}. It is also possible to create this frame manually using \code{\link{modelRP}}.
}
}
\seealso{
\code{\link{modelRP}},\code{\link{CreateDatabase}}
}
\examples{
library(DoOR.data)
cas <- c("71-36-3","123-92-2","79-09-4")
data(response.matrix)
result <- findRespNorm(cas, responseMatrix = response.matrix)
}
\keyword{ data }
