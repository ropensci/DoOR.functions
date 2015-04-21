#' Generates a model response and merge data in given sequence
#' 
#' Generates a model response and merge data in given sequence.
#' 
#' 
#' @param data data frame; odorant response data, e.g. Or22a.
#' @param SEQ character vector; containing the names of studies indicating
#' given sequence for merging data.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' data(Or35a)
#' data(response.range)
#' SEQ <- c("Hallem.2006.EN","Kreher.2008.EN","Yao.2005.WT")
#' selected.merg <- modelRPSEQ(Or35a, SEQ = SEQ)
#' 
modelRPSEQ <-
function(data,SEQ) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# modelRPSEQ.R:
#################

# merges studies in a given sequence (determined by the user or by exhaustive enumeration and choosing the optimal sequence)


# input parameters:
####################


# data  : data frame; odorant response data for a given receptor, e.g. Or22a
# SEQ 	: character vector; contains the names of studies that measured this receptor in a specific order (the merging sequence)

# output is a numeric vector: response values

{
	nv  	    <- as.numeric( c( (default.val("num.charColumns")+1):dim(data)[2] ) ) # positions of columns that contain odor response vectors
	name.stud   <- names(data)[nv]
	pda 	    <- apply(as.data.frame(data[, nv]), 2, DoORnorm) # processing data
	mseq 	    <- match(SEQ, name.stud) # match given sequence to the column names of response data
	first.study <- SEQ[1] 
	rest.study  <- SEQ[-match(first.study,SEQ)]

	# start merging following the given sequence
	y <- pda[,first.study]

	for (i in rest.study) 
	{
		x 	  <- pda[,i]
		ind 	  <- calModel(x,y)
		projected <- projectPoints(x,y)
		res 	  <- rep(NA, length = dim(pda)[1])

		# the output of projectPoints is a list with odor responses, either observed in both studies, or only in one study.
		res[projected$Double.Observations$ID] <- projected$Double.Observations$NDR
		if (is.data.frame(projected$Single.Observation)) 
		{
            		res[projected$Single.Observation$ID] <- projected$Single.Observation$NDR
        	}
		
                y <- res
	
         } # END merging

return(y)
}
