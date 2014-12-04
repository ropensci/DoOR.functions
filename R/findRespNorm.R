findRespNorm <-
function(CAS, zero= default.val("zero"), responseMatrix = default.val("response.matrix"))

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# findRespNorm.R:
##################

## given a chemical, get normalized receptor responses from all studies in the database


# input parameters:
####################

#  CAS 		: character vector; one or more odors.
#  zero 	: character string; The default is "SFR", i.e. the spontaneous firing rate.
#  responseMatrix 	: a data frame; as e.g. "mast" that is loaded by modelRP. It is also possible to create this frame manually using modelRP


## output is a data frame containing normalized responses for the given odors.

{

	Or.Names <- names(responseMatrix)
	n	 <- dim(responseMatrix)[2]
	pr.mast  <- responseMatrix

	for (j in 1:n)
	{
		if (is.na(which(!is.na(pr.mast[,j]))[1])) { next }
		else 
		{
			# reset the response data by scaling the "zero" to 0, the default "zero" is spontanous firing rate 
			if (is.na(pr.mast[1,j])) { mzero <- 0 }
			else 			   { mzero <- pr.mast[1,j] }
			pr.mast[,j] <- resetSFR(pr.mast[,j], mzero)	
		}
	}
	
	mp  <- match(CAS,rownames(pr.mast))
	res <- data.frame(ORs 	   = rep(colnames(pr.mast),each=length(CAS)),
			  Odor 	   = rep(CAS,length(Or.Names)),
			  Response = c(as.matrix(pr.mast[mp,])))
return(res)
}
