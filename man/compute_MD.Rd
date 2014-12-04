\name{compute_MD}
\Rdversion{1.1}
\alias{compute_MD}
\title{
compute the mean distance
}
\description{
compute the mean distance between observations and projected points on fitted curve.
}
\usage{
compute_MD(x, y, range.X, parms, fun)
}
\arguments{
  \item{x, y}{
data vectors; in which to evaluate the parameters in given formula.
}
  \item{range.X}{
numeric vector, defining the range of x, where the model function will be applied.
}
  \item{parms}{
parameters of model function.
}
  \item{fun}{
model function
}
}
\details{
This function is used to compute the mean distance between observations and their cloest points on fitted curve. Output is list containing the mean distance value $MD and a data frame $data showing the coordinates of observations, projected points and the distances between them.
}
\author{
Shouwen Ma <\email{shouwen.ma@uni-konstanz.de}>
}
\examples{
library(DoOR.data)
loadRD()
x<-DoORnorm(Or23a[,'Hallem.2004.EN'])
y<-DoORnorm(Or23a[,'Hallem.2006.EN'])
comb.xy     <-  na.omit(cbind(x, y))
range_x     <-  range(comb.xy[,'x'])
range_y     <-  range(comb.xy[,'y'])
interval.X  <-  c(-1,2)
model_parameters <- lm(x ~ y)
linear_parameters <- model_parameters$coef
names(linear_parameters) <- c("Intercept", "Slope")
bottom.x 	<- as.vector(modelfunction_linear(range_y[1],linear_parameters) )
top.x    	<- as.vector(modelfunction_linear(range_y[2],linear_parameters) )

range.X <- c(max(c(0, bottom.x, range_x[1])), min(c(1, top.x, range_x[2])))
MD <- compute_MD(x=x, y=y, range.X = range.X, parms = linear_parameters,fun = modelfunction_linear)
}
\keyword{ math }
