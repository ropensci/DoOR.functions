\name{backProject}
\Rdversion{1.1}
\alias{backProject}
\title{
project the model response values back to tested values
}
\description{
project the model response values back to tested values
}
\usage{
backProject(cons.data, bp.data, tag.odor, tag.cons.data, tag.bp.data)
}
\arguments{
  \item{cons.data}{
a data frame; containing model response values.
}
  \item{bp.data}{
a data frame; containing the data will be back projected.
}
  \item{tag.odor}{
a character string; indicating which row will be used for matching two data.
}
  \item{tag.cons.data}{
a character string; specifying which column is model responses in "con.data".
}
  \item{tag.bp.data}{
a character string; specifying which column in "bp.data" is the data that to be back projected.
}
}
\details{
	\code{\link{backProject}} is used to project the model response values back to tested values. 
	The process of back projection is following serval steps: 
   \itemize{
	\item 1. normalize two data set in values from 0 to 1; 
	\item 2. plot "bp.data" against "cons.data"; 
	\item 3. choose the best fitting model with the lowest "MD" value; 
	\item 4. project the consensus values from "x" onto fitted line, those projected Y coordinates are normalized back projected values; 
	\item 5. estimate the parameters ("intercept" and "slope") between unnormalized data and normalized data using linear regression; 
	\item 6. rescale all projected Y coordinates with the parameters.
   }
}
\value{
	Output of \code{\link{backProject}} is a list containing a numeric vector ($rescale) and a data frame ($output).
	"rescale" is used to rescale the normalized data to original scale. "output" contains 
	the whole "bp.data", and consensus value from "cons.data", "projected.Y" (normalized back 
	projected data) and rescaled back projected data "bp.data".
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\seealso{
	\code{\link{DoORnorm}},\code{\link{projectPoints}}, \code{\link{calModel}}, 
	\code{\link{modelfunction}}
}
\examples{

}
\keyword{ data }
