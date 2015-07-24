#' projects data points onto the curve defined by the model function
#' 
#' For internal use in the merging process (see also \code{\link{modelRP}}). The
#' model function is choosen by \code{\link{calModel}}. 
#' \code{\link{projectPoints}} then projects the data points from the datasets 
#' to be merged onto the curve defined by the model function. It computes the 
#' closest distance from a data point to a point on the curve by numerical 
#' optimisation.
#' 
#' A list with two data frames "double.observations" and "single.observations" is
#' returned, which give the coordinates of double observations (defined as 
#' (x,y)) and coordinates of single observation (defined as (x,NA) or (NA,y)). 
#' Both data frames contain seven columns: "ID" indicating the original position
#' of data x and y; "x", "y" indicating the coordinate of observation; "X", "Y" 
#' indicating the coordinate of projected point on the functional line; 
#' "distance" indicating the distances between \code{(xmin, f(xmin))} and all 
#' points on the functional line; "NDR" indicating the normalized distances 
#' across all the distance values.
#' 
#' @param x,y numeric vectors of data values; coordinate vectors of points to 
#'   plot, the coordinates can contain \code{NA} values.
#' @param xylim numeric vectors; x, y limits of the plot.
#' @param best.model a list; containing the parameters, function, inverse 
#'   function, Leibniz's notation for distance calculation and MD value. if 
#'   missing, the best model will be generated automatically.
#' @param plot logical; If \code{FALSE}, plotting is suppressed. Default is 
#'   \code{FALSE}.
#' @param points_cex a numerical value; giving the magnification level of 
#'   symbols relative to the default size.
#' @param title logical; If \code{TRUE}, title is shown. Default is 
#'   \code{FALSE}.
#' @param \dots further graphical parameters
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @seealso \code{\link{calModel}}, \code{\link{optimize}},
#'   \code{\link{integrate}}
#' @export
#' @examples
#' 
#' library(DoOR.data)
#' data(Or23a)
#' x <- DoORnorm(Or23a[,'Hallem.2004.EN'])
#' y <- DoORnorm(Or23a[,'Hallem.2006.EN'])
#' projectPoints(x = x, y = y, plot = TRUE)
#' 
projectPoints <- function(x, y, xylim, best.model, 
                          plot = default.val("plot"), 
                          points_cex = default.val("points.cex"),
                          title = default.val("title"), 
                          ...) { 
  
  
  # subfunctions:
  
  # returns -> 	0: no data
  #             1: available on x
  #             2: available on y
  #             3: available in both datasets
  available <- function(x) {return(sum(which(sapply(x,is.na) == FALSE))) }
  
  
  
  
  ## main program starts here:
  
  # compute the chosen fitting function (see calModel.R)
  if (missing(best.model)) { best.model <- calModel(x=x,y=y,select.MD = default.val("select.MD")) }
  
  # find overlap between datasets
  comb.xy   <-  cbind(x,y)
  overlap   <-  apply(comb.xy,1,available)
  
  if (is.na(which(overlap==3)[1])) { return(message('There is no observation between two data sets.')) }
  
  x_y  <-  na.omit(comb.xy)				
  q    <-  1:length(x)
  qq   <-  match(q,attr(x_y,"na.action"))
  ID   <-  which(is.na(qq))
  
  Coor_x 	   <- data.frame(ID = which(overlap == 1), x=comb.xy[which(overlap == 1),'x'], y = comb.xy[which(overlap == 1),'y']) 	# only available on x
  Coor_y     <- data.frame(ID = which(overlap == 2), x=comb.xy[which(overlap == 2),'x'], y = comb.xy[which(overlap == 2),'y'])  	# only available on y
  doubleCoor <- data.frame(ID = which(overlap == 3), x=comb.xy[which(overlap == 3),'x'], y = comb.xy[which(overlap == 3),'y'])  	# double coordinate data points (available in both studies)
  
  # ranges of observations x and y.
  range_x <- range(doubleCoor[,'x'])
  range_y <- range(doubleCoor[,'y'])
  
  # construct the best fitting interval function, comprising the result of calModel() (stored in "best.model")	
  
  
  # function.name : character; the name of chosen model
  # parms 	: numeric vector; the corresponding parameters
  # ff 		: expression; the chosen model (fitting) function
  # range.X 	: numeric vector; the range of x axis for the middle function
  # ff_inverse 	: expression; the corresponding inverse function
  # dsdx 		: expression; Leibniz's notation for distance calculation on the functional line 
  # MD.value 	: numeric; median distance
  
  function.name   <- names(best.model)
  parms           <- best.model[[1]]$parameters
  ff              <- best.model[[1]]$fun
  range.X         <- best.model[[1]]$range
  ff_inverse      <- best.model[[1]]$inverse.fun
  dsdx            <- best.model[[1]]$dsdx
  MD.value        <- best.model[[1]]$MD
  
  if (function.name == "no.fitted.model")	
    return(message('Data sets can not been fitted by any of optional models.')) 
  
  #  define the range of frame for middle function. (e.g. range.X, range.Y)
  range.Y <- sapply(range.X, function(x) intervalFun(x, range.X, middleFun = ff, parms))
  
  
  # project the double observation values onto the respective closest points on the curve (fitting function)
  # this is done numerically with comp.projected.double.coord()
  interval.X  <- default.val("interval.X") 	# determine the interval for searching.
  projected_double_coord  <- numeric()
  
  for (i in 1:dim(x_y)[1]) 
  {
    w <- x_y[i,]
    
    ff2 <- function(x) 
    {
      ry <- intervalFun(x, range.X, middleFun = ff, parms)
      return((w[1] - x)^2 + (w[2] - ry)^2)
    }
    
    opt.x <- optimize(ff2, interval.X, tol = 0.001)$minimum
    opt.y <- intervalFun(opt.x, range.X, middleFun = ff, parms)
    
    opt   <- c(opt.x, opt.y)					 
    projected_double_coord <- as.matrix(rbind(projected_double_coord, opt))
  }
  projected_double_coord <- data.frame(ID = ID, x = x_y[,1], y = x_y[,2], X = projected_double_coord[,1] , Y = projected_double_coord[,2])
  
  
  # project single observation values (only available in one study) onto the respective closest points on the curve
  
  Coor_X 	      <- as.matrix(Coor_x)
  Coor_Y        <- as.matrix(Coor_y)
  Coor_X[,'y']  <- sapply(Coor_X[,'x'], function(x) intervalFun(x, range.X, middleFun = ff, parms))
  Coor_Y[,'x']  <- sapply(Coor_Y[,'y'], function(x) intervalFun(x, range.X = range.Y, middleFun = ff_inverse, parms))
  
  single_coord_xy <- rbind(Coor_x, Coor_y)
  single_coord_XY <- rbind(Coor_X, Coor_Y)
  
  projected_single_coord <- data.frame(ID = single_coord_XY[,1], x = single_coord_xy[,'x'], y = single_coord_xy[,'y'],
                                       X = single_coord_XY[,'x'], Y = single_coord_XY[,'y'])
  
  projected_all <- rbind(projected_double_coord,projected_single_coord) 	# concatenate the double and single coordinates
  
  projected_all <- projected_all[order(projected_all[,'X']),] 		# sort projected_all (projected x values : lowest --> highest)
  
  
  ## distances on the curve are computed by integrating the intervals between neighbouring points + adding them up
  
  xmin <- projected_all[,'X'][which(projected_all[,'X']==min(projected_all[,'X'],na.rm=TRUE))] 	#  xmin is the starting point for this process
  
  if (length(xmin)>1) { xmin=xmin[1] }
  
  distance 	<- numeric() 	# vector holding all distances: for each point we compute the distance to the minimum value on the curve
  xi_1 		<- xmin 	# lower neighbour
  current_dist 	<- 0 		# distance between the pair of neighbours (lower and upper neighbour) currently regarded
  lower.dsdx 	<- function(x) sqrt(1+((ff(range.X[1], parms)-range.X[1]))^2)*x^0
  upper.dsdx 	<- function(x) sqrt(1+((ff(range.X[2], parms)-range.X[2]))^2)*x^0
  
  for (i in 1:dim(projected_all)[1])
  {
    xi <- projected_all[i,'X']
    if (is.na(xi)) 
    {
      current_dist <- NA 
      next
    }
    
    # decide whether we are in the linear regime or in the middle interval of the mixed (three-interval) fitting function
    # if we are in the middle interval, choose the fitting function as returned by calModel()
    # integrate to get the distance
    # ABOUT the RoundOff error during integrate. If integrating a nonlienar function from a value to Inf (or a value that bring the function infinite) , it will return a warning message 'roundoff                  error is detected in the extrapolation table'. To avoid that, the distance is calculated using linear function.
    
    if (xi_1 <= (range.X[1]) & xi <= (range.X[1]))
    {
      dis  <- integrate(lower.dsdx, xi_1, xi)$value
      xi_1 <- xi
      current_dist <- dis + current_dist
      distance     <- c(distance, current_dist)
      next
    }
    
    if (xi_1 < (range.X[1]) & xi > (range.X[1]))
    {	
      
      dis1 <- integrate(lower.dsdx, xi_1,range.X[1])$value
      dis2 <- integrate(function(x) {dsdx(x,parms)}, range.X[1], xi,rel.tol = 0.1e-6)$value
      dis 	     <- dis1 + dis2
      xi_1 	     <- xi
      current_dist <- dis + current_dist
      distance     <- c(distance, current_dist)
      next
    }
    if (xi_1 >= (range.X[1]) & xi <= (range.X[2])) 
    {
      dis  <- integrate(function(x) {dsdx(x,parms)}, xi_1, xi)$value	   	
      xi_1 <- xi 	# adding up, so that we always have the distance to the minimum
      current_dist <- dis + current_dist
      distance     <- c(distance, current_dist)
      next
    }
    if (xi_1 < (range.X[2]) & xi > (range.X[2]))
    {
      
      dis2 <- integrate(function(x) {dsdx(x,parms)}, xi_1, range.X[2])$value
      dis1 <- integrate(upper.dsdx, range.X[2], xi)$value
      dis  <- dis1 + dis2
      xi_1 	     <- xi
      current_dist <- dis + current_dist
      distance     <- c(distance, current_dist)
      next
    }
    
    if (xi_1 >= (range.X[2]) & xi >= (range.X[2]))
    {
      dis <- integrate(upper.dsdx, xi_1, xi)$value
      xi_1 <- xi
      current_dist <- dis + current_dist
      distance     <- c(distance, current_dist)
      next
    }		
  } # END for (i in 1:dim(projected_all)[1])
  
  projected_all <- cbind(projected_all, distance)
  
  
  # normalisation to [0,1]
  normalised_distances <- (projected_all[,'distance']-min(projected_all[,'distance'],na.rm=TRUE))/
    ((max(projected_all[,'distance'],na.rm=TRUE)-min(projected_all[,'distance'],na.rm=TRUE)))
  
  
  # distinguish again between single and double coordinate data points (we need information about this for the differential coloring in the plot)
  
  projected_all 	<- data.frame(projected_all,NDR=normalised_distances)
  res3 		<- projected_all[match(projected_double_coord[,1],projected_all$ID),]
  res4 		<- projected_all[match(projected_single_coord[,1],projected_all$ID),]
  
  if (plot == TRUE) 
  {
    range.projected_all.X=range(projected_all[,'X'],na.rm=TRUE)
    range.projected_all.Y=range(projected_all[,'Y'],na.rm=TRUE)
    range.projected_all.x=range(projected_all[,'x'],na.rm=TRUE)
    range.projected_all.y=range(projected_all[,'y'],na.rm=TRUE)
    
    if (!missing(xylim)) 
    { 
      plot(y~x, xlim=xylim, ylim=xylim, cex=points_cex, type="n", ...)
    }
    else
    { 
      plot(y~x,xlim=range(c(range.projected_all.X,range.projected_all.x)), ylim=range(c(range.projected_all.Y,range.projected_all.y)),cex=points_cex, type="n", ...)
    }
    
    seg_y=which(is.na(projected_all$x))
    
    if (!is.na(seg_y[1])) 
    {
      segments(x0=range.projected_all.X[1],y0=projected_all[seg_y,'y'],x1=projected_all[seg_y,'X'],y1=projected_all[seg_y,'y'],col='lightblue')
    }
    
    seg_x=which(is.na(projected_all$y))
    
    if (!is.na(seg_x[1])) 
    {
      segments(x0=projected_all[seg_x,'x'],y0=range.projected_all.Y[1],x1=projected_all[seg_x,'x'],y1=projected_all[seg_x,'Y'],col='yellow')
    }
    
    segments(projected_all[,'X'],projected_all[,'Y'],projected_all[,'x'],projected_all[,'y'],col='red')			
    points(projected_all[,'X'],projected_all[,'Y'],col="green",cex=points_cex)
    
    intervalCurve 	  <- numeric()
    seq_intervalCurve <- seq(from=range.projected_all.X[1],to=range.projected_all.X[2],length=100)
    for (i in 1:100) 
    {
      segm 		<- intervalFun(seq_intervalCurve[i],range.X,middleFun = ff,parms)
      intervalCurve 	<- c(intervalCurve, segm)
    }
    
    lines(intervalCurve~seq_intervalCurve,col='red')
    points(x,y)
    if (title==TRUE) 
    {
      title(paste("model:",names(best.model)), cex.main = 1)
      legend("topleft", c(paste("n=",dim(x_y)[1]), paste("MD=",round(best.model[[1]]$MD,6))),bty='n')
    }
    
    return(list(double.observations = res3, single.observations = res4, MD = MD.value))
    
  } # END if (plot == TRUE)
  
  else 
  {
    return(list(double.observations=res3,single.observations=res4, MD = MD.value))
  }
  
}
