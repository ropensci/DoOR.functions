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
    len.models  <- length(models)

    collect.MD <- numeric()
    for (i in 1:len.models)
    {
	collect.MD <- c(collect.MD, models[[c(i)]]$MD)
    }

    # select the best model from the ones tried above:

    if (select.MD == FALSE) 
    { 
	return(models) 
    }

    if (select.MD == TRUE) 
    {
	return(models[match(min(collect.MD,na.rm=TRUE),collect.MD)])
    }
}
