CreateDatabase <-
function(tag=default.val("tag"), select.MDValue=default.val("select.MDValue"), overlapValues = default.val("overlapValues") ) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# CreateDatabase.R :
####################

# Create the whole database; i.e. call the merging function modelRP() for all receptors


# input parameters:
###################

# tag 		 : character string; format for rownames; possibilities: tag="CAS", tag="CID", tag="Name"
# select.MDValue : numeric; threshold on the MAD, this is used to reject studies that do not align sufficiently well to the response model
#  overlapValues : numeric; a criterion using to refuse a data set that has not enough overlap value.


## output is an odors-by-receptors numeric matrix, i.e. the DoOR database

{

	Or.list  	<- loadRDList() 	# contains data for all receptors
	Or.Names 	<- names(Or.list)
	num_receptors 	<- length(Or.Names)	# how many receptors

	odors <- character()

	for (i in Or.Names) {
		pre.odors <- as.vector(Or.list[[i]][,tag])
		new_odors <- which(is.na(match(pre.odors,odors)))
		odors 	  <- c(odors,pre.odors[new_odors])
	}

	num_odors 	     <- length(odors)					# how many odors
	frame_data 	     <- matrix(NA,nrow=num_odors,ncol=num_receptors)	# empty matrix
	colnames(frame_data) <- Or.Names
	rownames(frame_data) <- odors

	for (i in Or.Names)
	{
		da <- Or.list[[i]]
	
        	# if no response data, fill in "NA" and skip
		if (dim(da)[2] <= default.val("num.charColumns")) { 
			print(paste(i, "is a empty data frame."))
			frame_data[, i] <- NA }
		else
		{
			merged 		   <- modelRP(da,select.MDValue, overlapValues)
			merged.responses   <- merged$model.response[,"merged_data"]
			merged.odors 	   <- as.vector(merged$model.response[,tag])
			match_odorsTOframe <- match(merged.odors, rownames(frame_data))
			frame_data[match_odorsTOframe, i] <- merged.responses
			print(paste(i, "has been merged."))
		}
	}
return(frame_data)
}
