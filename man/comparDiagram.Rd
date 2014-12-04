\name{comparDiagram}
\Rdversion{1.1}
\alias{comparDiagram}
\title{
Compare the results of two studies
}
\description{
Orderdered bar plots for two studies, allowing for an easy comparison of the two studies' results.
}
\usage{
comparDiagram(x, y, by.x, by.y, lim.y)
}
\arguments{
  \item{x, y}{
data frames; data x and y could be two different data frame or a same data frame.
}
  \item{by.x}{
character string; specifying a column in x.
}
  \item{by.y}{
character string; specifying a column in y.
}
  \item{lim.y}{
numeric vector; x-axis limit of data y.
}
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
library(DoOR.data)
data(Or22a)
data(response.range)
comparDiagram(x=Or22a, y=Or22a, by.x="Hallem.2006.EN", by.y="Pelz.2006.AntEC50", lim.y=c(-0.5,7))
}
\keyword{ iplot }
