\name{default.val}
\Rdversion{1.1}
\alias{default.val}
\title{
default values for DoOR functions
}
\description{
\code{default.val} is used to return default values for DoOR functions.
}
\usage{
default.val(DoOR_default)
}
\arguments{
  \item{DoOR_default}{
a character string; indicating which argument is to be returned for DoOR functions.
}
}
\details{
There are six categories for default value. real number, integer, logical, NULL, character string and character vector.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
default.val(DoOR_default = "select.MD")
}
\keyword{ data }
