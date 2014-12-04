\name{tuningBreadth}
\Rdversion{1.1}
\alias{tuningBreadth}
\title{
Receptor tuning breadth
}
\description{
Display the distribution of a receptor's responses (pyramid plot).
}
\usage{
tuningBreadth(x, ...)
}
\arguments{
  \item{x}{
a numeric vector
}
  \item{\dots}{
further graphical parameters, see \code{\link{par}}
}
}
\details{
This function is used to visualise the response spectrum of a receptor. Generalists have a distribution with broad tails, while specialist receptors have a distinct peak that decreases sharply towards the tails.
}
\references{
Hallem E. A. & Carlson J. R., 2006, 
	\emph{Coding of odors by a receptor repertoire}, Cell, 125:143-160 \cr 
	\url{http://www.cell.com/content/article/abstract?uid=PIIS0092867406003631}
}
\seealso{
\code{\link{barplot}}
}
\examples{
library(DoOR.data)
data(Or22a)
x=Or22a[,"Hallem.2006.EN"]
x<-na.omit(x)
tuningBreadth(x,las=2,main="Tuning Curve:Hallem.2006.EN")
}
\keyword{ iplot }
