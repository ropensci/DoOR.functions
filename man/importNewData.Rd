\name{importNewData}
\Rdversion{1.1}
\alias{importNewData}
\title{
import new data into DoOR
}
\description{
import new data and update the weight, response range and receptor names
}
\usage{
importNewData(file.name, file.format, dataFormat = default.val("data.format"),
			odor.data = default.val("odor.data"), 
			weightGlobNorm = default.val("weight.globNorm"), 
			responseRange = default.val("response.range"), 
			receptors = default.val("ORs") )
}
\arguments{
  \item{file.name}{
character string; the name of given file that contains response values of one or more odorant receptors.
}
  \item{file.format}{
character string; the format of given file, either ".txt" or ".csv"
}
  \item{dataFormat}{
data frame; a data frame does not contain any response value but odorant information.
}
  \item{odor.data}{
data frame; a data frame that contains the odorant information.
}
  \item{weightGlobNorm}{
data matrix; indicates whether given receptor has been measured by given study.
}
  \item{responseRange}{
data frame; contains the information about response range of each study and how many odors have been measured in each study.
}
  \item{receptors}{
data frame, contains the receptor and ORN names and their expression.
}
}
\details{
\code{\link{importNewData}} is used to import new data into database. If the data contains a new receptor or ORN, then build a new data frame for this receptor or ORN. If the data contains a receptor that is already present in database, then merge the imported data into old data frame with function \code{\link{combData}}. The information (e.g. response range, how many receptors and odors were measured from given study) will be integrated into data "response.range", "ORs" and "weight.globNorm".
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
# import new data named "odorantResponses_Orx.txt" into database and update the support data.
# library(DoOR.data)
# importNewData(file.name="odorantResponses_Orx", file.format=".txt")
}
\keyword{ data }
