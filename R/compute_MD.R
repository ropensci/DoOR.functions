compute_MD <-
function(x, y, range.X, parms, fun)
{

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## compute the mean distance between observations and their cloest points on fitted curve.

# x and y 	: data vectors; in which to evaluate the parameters in given formula.
# range.X 	: numeric vector, defining the range of x, where the model function will be applied.
# parms   	: parameters of model function.
# fun 		: model function

## output is list containing the mean distance value $MD and a data frame $data showing the coordinates of observations, projected points and the distances between them.

	x_y 	    <- na.omit(cbind(x, y))
	pp  	    <- numeric()
        interval.X  <- default.val("interval.X")
	for (i in 1:dim(x_y)[1]) 
	{
		w   <- x_y[i, ]          
		ff2 <- function(x) 
		{
			ry <- intervalFun(x, range.X = range.X, middleFun = fun, parms)
			return((w[1] - x)^2 + (w[2] - ry)^2)
		}
		opt.x <- optimize(ff2, interval.X, tol = 1e-04)$minimum
		opt.y <- intervalFun(opt.x, range.X, middleFun = fun, parms)
		opt   <- c(opt.x, opt.y)
		pp    <- rbind(pp, opt,deparse.level = 0)
	}
	colnames(pp) <- c("X", "Y")
	dist 	     <- sqrt(apply((pp[,c("X", "Y")] - x_y)^2, 1, sum))
	MD 	     <- mean(dist)
	return.data  <- data.frame(x_y, pp, distance = dist )
	return(list(MD = MD, data = return.data))
}
