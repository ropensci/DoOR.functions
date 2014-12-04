\name{modelRP}
\Rdversion{1.1}
\alias{modelRP}
\title{
Generates a model response
}
\description{
Runs the DoOR algorithm, that merges all measurements for one receptor into a common response model.
}
\usage{
modelRP(da, select.MDValue = default.val("select.MDValue"), overlapValues = default.val("overlapValues"), responseRange = default.val("response.range"), weightGlobNorm = default.val("weight.globNorm"), glob.normalization = default.val("glob.normalization"), plot = default.val("plot"))
}
\arguments{
  \item{da}{
data frame; a selected receptor containing measured responses from studies.
}
  \item{select.MDValue}{
numeric; threshold on the MD for rejecting a fit.
}
  \item{overlapValues}{
numeric; a criterion using to refuse a data set that has not enough overlap value.
}
  \item{responseRange}{
data frame; contains response ranges for all studies.
}
  \item{weightGlobNorm}{
data frame; a binary data matrix; 1 indicates given odor has been measured in given study, NA indicates NOT.
}
  \item{glob.normalization}{
logical; default is \code{TRUE}; performs a global normalization for the model response. Otherwise (\code{FALSE}) response values will be given in value from 0 to 1.
}
  \item{plot}{
logical; If \code{FALSE}, plotting is suppressed. Default is \code{FALSE}.
}
}
\details{
  Merging a data is processed by following:
	\enumerate{
	   \item Normalize all response data in value [0,1].
	   \item Compute the correlation between studies and selected the best pair using \code{\link{selectModel}}.
	   \item Merge the first pair using function \code{\link{projectPoints}}.
	   \item Add other datasets if the correlation between the growing model response and the new dataset is below the correlation threshold (select.MDValue). Datasets excluded based on this criterion will be appended in a separate list.
	  }
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
library(DoOR.data)
data(Or35a)
data(weight.globNorm)
data(response.range)
# merge data without R2 selection
RP.Or35a <- modelRP(Or35a, plot = TRUE)
}
\keyword{ data }
