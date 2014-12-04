\name{showOdor}
\Rdversion{1.1}
\alias{showOdor}
\title{
Link to PubChem
}
\description{
obtain information on a chemical from PubChem
}
\usage{
showOdor(CID, CAS, odor.data)
}
\arguments{
  \item{CID}{
a character string; the compound identity (CID) in pubchem.
}
  \item{CAS}{
a character string; the CAS number.
}
  \item{odor.data}{
data frame; chemical data of odors.
}
}
\details{
Send a query to PubChem. The result is displayed using the default WWW-browser.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\seealso{
\code{\link{browseURL}}
}
\examples{
library(DoOR.data)
data(odor)
showOdor(CID=222,odor.data=odor)
showOdor(CAS="64-19-7",odor.data=odor)
}
\keyword{ data }
