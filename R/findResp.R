findResp <-
function(CAS, responseRange = default.val("response.range"), Or.list)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# findResp.R:
#############

# given a chemical, get original receptor responses from all studies in the database.


#input parameters:
##################

#  CAS 		  : CAS number of query odor
#  responseRange  : response ranges of studies
#  Or.list 	  : a list containing reponse data for all available receptors

## output is a data frame containing response values for the given odor across receptors from all available studies. 

{

	studies  <- responseRange[,"study"]
    	Or.Names <- names(Or.list)
    	res 	 <- numeric()

    	for (i in 1:length(Or.Names)) 
	{
        	match_odor <- match(CAS, as.character(Or.list[[i]]$CAS))
	        pres1 	   <- Or.list[[i]][match_odor, ]
		valueCol   <- as.numeric(which(sapply(pres1, is.numeric)))

		if (is.na(valueCol[1]))
		{
			pres<-data.frame(ORs = Or.Names[i], studies = NA, Odor = CAS, Response = NA )
		}
		else 
		{
			pres<-data.frame(ORs = Or.Names[i], studies = colnames(pres1)[valueCol], Odor = CAS, Response = c(as.matrix(pres1[, valueCol])))
		}
		res<-rbind(res,pres)
	}

return(res)
}
