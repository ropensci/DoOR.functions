\name{CreateDatabase}
\Rdversion{1.1}
\alias{CreateDatabase}
\title{
Compose a Response Matrix of All Odor Receptors
}
\description{
computes the complete response model for all receptors in the database  (calls \code{\link{modelRP}} for all receptors)
}
\usage{
CreateDatabase(tag = default.val("tag"), select.MDValue = default.val("select.MDValue"), overlapValues = default.val("overlapValues"))
}
\arguments{
  \item{tag}{
character string; format for rownames; possibilities: tag="CAS", tag="CID", tag="Name"
}
  \item{select.MDValue}{
a numeric; threshold on the MD, this is used to reject studies that do not align sufficiently well to the response model
}
  \item{overlapValues}{
numeric; a criterion using to refuse a data set that has not enough overlap value.
}
}
\seealso{
\code{\link{modelRP}}
}
\examples{
library(DoOR.data)
loadRD()
# mydatabase <- CreateDatabase()
}
\keyword{ data }
