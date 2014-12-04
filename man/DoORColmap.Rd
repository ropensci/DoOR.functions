\name{DoORColmap}
\Rdversion{1.1}
\alias{DoORColmap}
\title{
Assign a color code for consensus response
}
\description{
Assign a color code for for negative and positive value which is in range of [0, 1]
}
\usage{
DoORColmap(x, col.extrem = default.val("col.extrem"), color.scale = FALSE)
}
\arguments{
  \item{x}{
numeric vector; consensus response data, which is in range of [0, 1].
}
  \item{col.extrem}{
character vector;  The default is c("blue", "red"), i.e. the color scale ranges from blue to red 
}
  \item{color.scale}{
logical; the default is FALSE, indicating that the result returns a color code for given value. If TRUE, the result returns the whole spectrum (255) color codes.
}
}
\value{
A character vector with elements of 7 or 9 characters, "#" followed by the red, blue, green and optionally alpha values in hexadecimal (after rescaling to 0 ... 255), see \code{\link{rgb}}.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\seealso{
\code{\link{rgb}}, \code{\link{colorRamp}}, \code{\link{ALimage}}, \code{\link{ORdotplot}}, \code{\link{PlotChemicals}}, \code{\link{PlotReceptors}}
}
\examples{
library(DoOR.data)
data(response.matrix)
RP.Or22a <- response.matrix[,"Or22a"]
cols <- DoORColmap(RP.Or22a)
# return the color scale
colscale <- DoORColmap(x=null,color.scale=TRUE)
}
\keyword{ color }
