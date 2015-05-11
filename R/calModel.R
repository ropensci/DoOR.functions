#' select the best model function
#' 
#' \code{calModel} is used to return the best model function that represent the
#' relationship between responses from study x and y..
#' 
#' \code{calModel} chooses the best model function from following: linear
#' (\code{\link{modelfunction_linear}}), exponential function
#' (\code{\link{modelfunction_exp}}), sigmoid
#' (\code{\link{modelfunction_sigmoid}}), asymptotic model with x intercept
#' (\code{\link{modelfunction_asympOff}}), asympototic model with y intercept
#' (\code{\link{modelfunction_asymp}}) and their inverse model
#' functions:(\code{\link{inverse_modelfunction_linear}}),
#' (\code{\link{inverse_modelfunction_exp}}),
#' (\code{\link{inverse_modelfunction_sigmoid}}),
#' (\code{\link{inverse_modelfunction_asympOff}}) and
#' (\code{\link{inverse_modelfunction_asymp}}).
#' 
#' @param x,y data vectors from study x and y (can contain NA)
#' @param select.MD logical; if TRUE, only the best model function (in terms of
#' MD) will be returned.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @seealso \code{\link{SSlogis}},\code{\link{SSasympOff}},
#' \code{\link{SSasymp}}
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' data(Or35a)
#' x <- DoORnorm(Or35a[,5])
#' y <- DoORnorm(Or35a[,8])
#' calM_xy <- calModel(x, y, select.MD = default.val("select.MD"))
#' 
calModel <-
function (x, y, select.MD = default.val("select.MD") ) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# calModel.R : 
################

# tests which model is best suited to represent the relationship between the (odor response) values from study x and y




# parameters:
##############

# x,y 	    :  data vectors from study x and y (can contain NA)  
# select.MD :  logical; if TRUE, only the best model function (in terms of MD) will be returned. 

{
    # program starts here
    comb.xy     <-  na.omit(cbind(x, y))
    if (dim(comb.xy)[1]==0) { stop("There is no observation between x and y.") }

    # compute the parameters of 5 optional models with and without inverse function (see modelfunction.R).
    models 	<- modelfunction(x,y)

    # select the best model from the ones tried above:

    if (select.MD == FALSE) { 
	    return(models) 
    }

    if (select.MD == TRUE) {
      collect.MD <- sapply(models, "[[", "MD")
      return(models["inv.linear"])
    }
}
