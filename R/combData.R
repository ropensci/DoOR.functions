combData <-
function(data1, data2, by.data2, assigned.name) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# combData.R:
#############

# Merge two datasets into one (by CAS number)


#input parameters:
###################


#  data1 	 : data frame; 
#  data2 	 : data frame; 
#  by.data2 	 : character string; a column name in "data2", specifies which column will be used for merging (typically "CAS" to merge by CAS-number)
#  assigned.name : character string; name to be assigned to the colum (see output) with the merged odor set


## output is a data frame containing all columns from data1 plus a column containing the merged odor set (CAS-numbers) from data1 and data2
{
	
	
	new.odor <- which(is.na(match(data2$CAS,data1$CAS)))

	# if no new odor
	if (is.na(new.odor[1])) 
        {
		output 	  <- data1
		matchOdor <- match(data2$CAS, output$CAS)
		if (missing(assigned.name)) { assigned.name <- by.data2 }
		output[matchOdor, assigned.name] <- data2[, by.data2]
	}
	else 
        {
		data_NewOdor <- data2[new.odor,]
		l.d1 <- length(data1[,"CAS"])
		l.d2 <- length(data_NewOdor[,"CAS"])
		nrow.new <- l.d1 + l.d2
		first_NewOdor <- data1[(l.d1+1),]
		output <- data1
	
        	# add new odors to the frame one by one
		for (i in seq(l.d2)) 
                {
			next_NewOdor 	     <- first_NewOdor
			next_NewOdor[,"CAS"] <- as.character(data_NewOdor[i,"CAS"])
			output 		     <- rbind(output,next_NewOdor)
			rownames(output)     <- seq(l.d1+i)
		}

		if (missing(assigned.name)) { assigned.name <- by.data2 }
	
           	# add response values of new odors to the output one by one
		for (i in seq(l.d2)) 
		{
			output[l.d1+i,assigned.name] <- data_NewOdor[i,by.data2]
		}
	
        	matchOdor <- match(data2$CAS,output$CAS)
		output[matchOdor, assigned.name] <- data2[, by.data2]
	}


return(output)
}
