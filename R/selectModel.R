selectModel <-
function(candidate, 
			data_candidate, 
			merged_data,
			overlapValues = default.val("overlapValues"),
			merged = default.val("merged") ) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# selectModel.R :
#################

# tries several fitting functions (e.g. exponential, sigmoidal, ...) using "calModel.R" and selects the one with minimum "MD" value. 


# input parameters:
####################

#  candidate 	  : a character vector; names of studies
#  data_candidate : a data frame; odorant response data (without odorant information etc.)
#  merged_data 	  : numeric vector; merged data
#  overlapValues  : numeric; a criterion using to refuse a data set that has not enough overlap value.
#  merged 	  : logical; if merged is TRUE, calculate models between merged_data (the growing response model) and candidate data (a study). 
#                            If FALSE, calculate models between candidates (studies).


# output: character   : "selected.x", "selected.y"  (names of the studies on x- and y- axis)  
#         "best.model": the best model (in the format returned by calModel.R) 

{
   
    initial 	  <- list(model.name = "no.fitted.model", cal.parameters = NA, MD = default.val("select.MDValue"))
    curr.model    <- list(initial = initial)
    best.model    <- curr.model
    selected.y    <- character()
    selected.x    <- character()
    len.cand 	  <- length(candidate)
    nam.data.cand <- colnames(data_candidate)
    seq.cand 	  <- match(candidate,nam.data.cand)


    if (merged == TRUE) { seq.i <- 1 }
    else 		{ seq.i <- 1:(len.cand-1) }

    for (i in seq.i) 
    {
	if (merged == TRUE)
    	{
		y <- merged_data
		i <- 0
  	}
	else { y <- data_candidate[,i] }

	for (j in (i+1):len.cand) 
	{
                # different data sets - map them
		x   <- data_candidate[,seq.cand[j]]
		x_y <- na.omit(cbind(x,y))

                # skip if
                # if there are no overlapping data points (i.e. disjunct odor sets)
                # or if there are less than "overlapValues" (too few to map)

		if (dim(x_y)[1] < overlapValues ) 
		{
     	        	next
     	        }
              
                # skip also if
                # slope between x and y is 0 (horizontal line) or NA (vertical line).
		if (is.na( lm(y~x)$coef[2] ) | lm(y~x)$coef[2] == 0) 
		{
     	        	next
     	        }
		curr.model <- suppressWarnings(calModel(x = x, y = y, select.MD = TRUE))

                # update best.model if a better fitting was found
		if (curr.model[[1]]$MD < best.model[[1]]$MD)
		{
			best.model <- curr.model
			selected.x <- candidate[j]
			if (merged == FALSE) { selected.y <- candidate[i] }
			else { selected.y <- "merged_data"}
		}
	
	} # END for (j in (i+1):len.cand)
    } # END for (i in seq.i)
    

return(list(best.model = best.model, selected.x =selected.x, selected.y = selected.y))
}
