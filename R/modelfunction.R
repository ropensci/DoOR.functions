# estimate the parameters for all optional model function
#
# estimate the parameters for all optional model function
#
# This function is used to estimate the parameters for optional model
# functions with lm() for linear model and nls() for nonlinear function:
# linear (\code{\link{modelfunction_linear}}), exponential
# (\code{\link{modelfunction_exp}}), sigmoid (\code{\link{SSlogis}}),
# asymptotic model with x intercept (\code{\link{SSasympOff}}), asympototic
# model with y intercept (\code{\link{SSasymp}}) and their inverse function,
# \code{\link{modelfunction_linear_inverse}},
# \code{\link{modelfunction_exp_inverse}},
# \code{\link{modelfunction_sigmoid_inverse}},
# \code{\link{modelfunction_asymp_off_inverse}},
# \code{\link{modelfunction_asymp_inverse}}. The output is a list containing
# estimated parameter, function and inverse function expression, applied range
# for middle function, Leibniz's notation for computing curvic length and mean
# distance value "MD" of all optional model function.
#
# @param x,y data vectors, in which to evaluate the parameters in given
# formula.
# @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
# @keywords math
# @examples
#
# library(DoOR.data)
# load_door_data(nointeraction = TRUE)
# x<-door_norm(Or23a[,'Hallem.2004.EN'])
# y<-door_norm(Or23a[,'Hallem.2006.EN'])
# MF_xy <- modelfunction(x=x,y=y)
#' @importFrom stats SSasymp SSasympOff SSlogis na.omit lm nls
modelfunction <- function(x, y) {

  ## main program starts here

  comb.xy     <-  na.omit(cbind(x, y))
  if (dim(comb.xy)[1]==0) { stop("There is no observation between x and y.") }

  range_x     <-  range(comb.xy[,'x'])
  range_y     <-  range(comb.xy[,'y'])
  interval.X  <-  door_default_values("interval.X")

  ################# Linear #################
  # linear model
  model_parameters  <- lm(y ~ x)
  linear_parameters <- model_parameters$coef
  names(linear_parameters) <- c("Intercept", "Slope")
  bottom.x 	<-
    as.vector(modelfunction_linear_inverse(range_y[1], linear_parameters))
  top.x    	<-
    as.vector(modelfunction_linear_inverse(range_y[2], linear_parameters))
  range.X <-
    c(max(c(0, bottom.x, range_x[1])), min(c(1, top.x, range_x[2])))
  if (linear_parameters["Slope"] < 0) {
    c.linear <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else {
    c.linear <- list(
      parameters  = linear_parameters,
      range 	     = range.X,
      fun         = modelfunction_linear,
      inverse.fun = modelfunction_linear_inverse,
      dsdx	       = dsdx_linear,
      MD          = compute_MD(
        x = x,
        y = y,
        range.X = range.X,
        parms = linear_parameters,
        fun = modelfunction_linear
      )$MD
    )
  }
  linear_parameters <- NA
  range.X	<- NA

  # inverse linear
  model_parameters <- lm(x ~ y)
  linear_parameters <- model_parameters$coef
  names(linear_parameters) <- c("Intercept", "Slope")
  bottom.x 	<-
    as.vector(modelfunction_linear(range_y[1], linear_parameters))
  top.x    	<-
    as.vector(modelfunction_linear(range_y[2], linear_parameters))
  range.X <-
    c(max(c(0, bottom.x, range_x[1])), min(c(1, top.x, range_x[2])))
  if (linear_parameters["Slope"] < 0) {
    c.linear.inv <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else {
    c.linear.inv <- list(
      parameters  = linear_parameters,
      range       = range.X,
      fun         = modelfunction_linear_inverse,
      inverse.fun = modelfunction_linear,
      dsdx        = dsdx_linear_inverse,
      MD          = compute_MD(
        x = x,
        y = y,
        range.X = range.X,
        parms = linear_parameters,
        fun = modelfunction_linear_inverse
      )$MD
    )
  }
  linear_parameters <- NA
  range.X		  <- NA
  ### END linear ###


  ################# expo #################
  # exponential model
  model_parameters <- try(nls(y ~ SSexpo(x, a, b, c)), silent = TRUE)
  if (inherits(model_parameters, "try-error"))
  {
    c.expo <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else {
    parms 	<- model_parameters$m$getAllPars()
    bottom.x 	<-
      as.vector(modelfunction_exp_inverse(range_y[1], parms))
    top.x    	<-
      as.vector(modelfunction_exp_inverse(range_y[2], parms))
    range.X <-
      c(max(c(0, bottom.x, range_x[1]), na.rm = TRUE), min(c(top.x, 
                                                             range_x[2], 1), 
                                                           na.rm = TRUE))
    c.expo <- list(
      parameters  = parms,
      range 	   = range.X,
      fun 	   = modelfunction_exp,
      inverse.fun = modelfunction_exp_inverse,
      dsdx	   = dsdx_exp,
      MD          = compute_MD(
        x = x,
        y = y,
        range.X = range.X,
        parms = parms,
        fun = modelfunction_exp
      )$MD
    )
  }
  parms   <- NA
  range.X	<- NA
  
  # inverse exponential model
  model_parameters <-
    try(nls(x ~ SSexpo(y, a, b, c)), silent = TRUE)
  if (inherits(model_parameters, "try-error")) {
    c.expo.inv <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else {
    parms 	     <- model_parameters$m$getAllPars()
    bottom.x     <- as.vector(modelfunction_exp(range_y[1], parms))
    top.x        <- as.vector(modelfunction_exp(range_y[2], parms))
    range.bottom <-
      max(c(0, bottom.x, range_x[1], parms["a"]), na.rm = TRUE)
    range.top    <- min(c(1, top.x, range_x[2]), na.rm = TRUE)
    range.X.inv  <- c(range.bottom, range.top)
    
    c.expo.inv   <- list(
      parameters  = parms,
      range 	 = range.X.inv,
      fun 	 = modelfunction_exp_inverse,
      inverse.fun  = modelfunction_exp,
      dsdx	 = dsdx_exp_inverse,
      MD           = compute_MD(
        x = x,
        y = y,
        range.X = range.X.inv,
        parms = parms,
        fun = modelfunction_exp_inverse
      )$MD
    )
    range.X.inv <- NA
  }
  parms 	    <- NA
  range.X.inv <- NA
  ### END expo ###
  
  
  ################# sigmoid #################
  # sigmoid function
  suppressWarnings(model_parameters <-
                     try(nls(y ~ SSlogis(x, Asym, xmid, scal)), silent = TRUE)
  )
  if (inherits(model_parameters, "try-error"))
  {
    c.sigmoid <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else
  {
    parms 	<- model_parameters$m$getAllPars()
    bottom.x 	<-
      as.vector(modelfunction_sigmoid_inverse(range_y[1], parms))
    top.x    	<-
      as.vector(modelfunction_sigmoid_inverse(range_y[2], parms))
    range.X <-
      c(max(c(0, bottom.x, range_x[1]), na.rm  =  TRUE), min(c(1, top.x, 
                                                               range_x[2]), 
                                                             na.rm  = TRUE))
    c.sigmoid <- list(
      parameters  = parms,
      range       = range.X,
      fun  	      = modelfunction_sigmoid,
      inverse.fun = modelfunction_sigmoid_inverse,
      dsdx 	      = dsdx_sigmoid,
      MD          = compute_MD(
        x  =  x,
        y  =  y,
        range.X = range.X,
        parms = parms,
        fun = modelfunction_sigmoid
      )$MD
    )
  }
  parms   <- NA
  range.X	<- NA
  
  # inverse sigmoid function
  suppressWarnings(
    model_parameters <-
      try(nls(x ~ SSlogis(y, Asym, xmid, scal)), silent = TRUE)
  )
  if (inherits(model_parameters, "try-error"))
  {
    c.sigmoid.inv <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else
  {
    parms 	     <- model_parameters$m$getAllPars()
    bottom.x       <-
      as.vector(modelfunction_sigmoid(range_y[1], parms))
    top.x    	     <-
      as.vector(modelfunction_sigmoid(range_y[2], parms))
    range.bottom <- max(c(0, bottom.x, range_x[1]), na.rm  =  TRUE)
    range.top    <-
      min(c(1, top.x, range_x[2], parms["Asym"]), na.rm  =  TRUE)
    range.X.inv  <- c(range.bottom, range.top)
    
    c.sigmoid.inv <- list(
      parameters  = parms,
      range 	  = range.X.inv,
      fun    	  = modelfunction_sigmoid_inverse,
      inverse.fun = modelfunction_sigmoid,
      dsdx 	  = dsdx_sigmoid_inverse,
      MD          = compute_MD(
        x  =  x,
        y  =  y,
        range.X = range.X.inv,
        parms = parms,
        fun = modelfunction_sigmoid_inverse
      )$MD
    )
    
  }
  parms 	    <- NA
  range.X.inv <- NA
  ### END sigmoid ###
  
  
  ################# Asymptotic model with an Offset #################
  # Asymptotic mode
  suppressWarnings(
    model_parameters <-
      try(nls(y ~ SSasympOff(x, Asym, lrc, c0)), silent = TRUE)
  )
  if (inherits(model_parameters, "try-error"))
  {
    c.asympOff <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else
  {
    parms 	<- model_parameters$m$getAllPars()
    bottom.x 	<-
      as.vector(modelfunction_asymp_off_inverse(range_y[1], parms))
    top.x    	<-
      as.vector(modelfunction_asymp_off_inverse(range_y[2], parms))
    range.X <-
      c(max(c(0, bottom.x, range_x[1]), na.rm  =  TRUE), min(c(1, top.x, 
                                                               range_x[2]), 
                                                             na.rm  = TRUE))
    c.asympOff <- list(
      parameters  	= parms,
      range 	= range.X,
      fun    	= modelfunction_asymp_off,
      inverse.fun 	= modelfunction_asymp_off_inverse,
      dsdx 	= dsdx_asymp_off,
      MD         	= compute_MD(
        x  =  x,
        y  =  y,
        range.X = range.X,
        parms = parms,
        fun = modelfunction_asymp_off
      )$MD
    )
    
  }
  parms   <- NA
  range.X	<- NA
  
  # inverse Asymptotic mode
  suppressWarnings(
    model_parameters <-
      try(nls(x ~ SSasympOff(y, Asym, lrc, c0)), silent = TRUE)
  )
  if (inherits(model_parameters, "try-error"))
  {
    c.asympOff.inv <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else
  {
    parms 	     <- model_parameters$m$getAllPars()
    bottom.x 	     <-
      as.vector(modelfunction_asymp_off(range_y[1], parms))
    top.x    	     <-
      as.vector(modelfunction_asymp_off(range_y[2], parms))
    range.bottom <- max(c(0, bottom.x, range_x[1]), na.rm  =  TRUE)
    range.top    <-
      min(c(1, top.x, range_x[2], parms["Asym"]), na.rm  =  TRUE)
    range.X.inv  <- c(range.bottom, range.top)
    
    c.asympOff.inv <- list(
      parameters  = parms,
      range 	   = range.X.inv,
      fun    	   = modelfunction_asymp_off_inverse,
      inverse.fun = modelfunction_asymp_off,
      dsdx 	   = dsdx_asymp_off_inverse,
      MD          = compute_MD(
        x  =  x,
        y  =  y,
        range.X = range.X.inv,
        parms = parms,
        fun = modelfunction_asymp_off_inverse
      )$MD
    )
    
  }
  parms 	    <- NA
  range.X.inv <- NA
  ### END Asymptotic model with an Offset ###
  
  
  ################# Asymptotic model #################
  # Asymptotic model
  suppressWarnings(
    model_parameters <-
      try(nls(y ~ SSasymp(x, Asym, R0, lrc)), silent = TRUE)
  )
  if (inherits(model_parameters, "try-error"))
  {
    c.asymp <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else
  {
    parms 	<- model_parameters$m$getAllPars()
    bottom.x 	<-
      as.vector(modelfunction_asymp_inverse(range_y[1], parms))
    top.x    	<-
      as.vector(modelfunction_asymp_inverse(range_y[2], parms))
    range.X <-
      c(max(c(0, bottom.x, range_x[1]), na.rm  =  TRUE), min(c(1, top.x, 
                                                               range_x[2]), 
                                                             na.rm = TRUE))
    c.asymp <- list(
      parameters  = parms,
      range 	    = range.X,
      fun 	    = modelfunction_asymp,
      inverse.fun = modelfunction_asymp_inverse,
      dsdx	    = dsdx_asymp,
      MD          = compute_MD(
        x  =  x,
        y  =  y,
        range.X = range.X,
        parms = parms,
        fun = modelfunction_asymp
      )$MD
    )
  }
  parms   <- NA
  range.X	<- NA
  #inverse Asymptotic model
  suppressWarnings(
    model_parameters <-
      try(nls(x ~ SSasymp(y, Asym, R0, lrc)), silent = TRUE)
  )
  if (inherits(model_parameters, "try-error"))
  {
    c.asymp.inv <-
      list(
        parameters = NA,
        range = NA,
        fun = NA,
        inverse.fun = NA,
        dsdx = NA,
        MD = NA
      )
  } else
  {
    parms 	     <- model_parameters$m$getAllPars()
    bottom.x 	     <-
      as.vector(modelfunction_asymp(range_y[1], parms))
    top.x    	     <-
      as.vector(modelfunction_asymp(range_y[2], parms))
    range.bottom <- max(c(0, bottom.x, range_x[1]), na.rm  =  TRUE)
    range.top    <-
      min(c(1, top.x, range_x[2], parms["Asym"]), na.rm  =  TRUE)
    range.X.inv  <- c(range.bottom, range.top)
    
    c.asymp.inv <- list(
      parameters  = parms,
      range 	= range.X.inv,
      fun 	= modelfunction_asymp_inverse,
      inverse.fun = modelfunction_asymp,
      dsdx	= dsdx_asymp_inverse,
      MD          = compute_MD(
        x  =  x,
        y  =  y,
        range.X = range.X.inv,
        parms = parms,
        fun = modelfunction_asymp_inverse
      )$MD
    )
    
  }
  parms 	    <- NA
  range.X.inv <- NA
  ### END Asymptotic model ###
  
  # output
  return(
    list(
      linear   	 = c.linear,
      inv.linear 	 = c.linear.inv,
      expo     	 = c.expo,
      inv.expo 	 = c.expo.inv,
      sigmoid  	 = c.sigmoid,
      inv.sigmoid  = c.sigmoid.inv,
      asympOff 	 = c.asympOff,
      inv.asympOff = c.asympOff.inv,
      asymp    	 = c.asymp,
      inv.asymp 	 = c.asymp.inv
    )
  )
}
