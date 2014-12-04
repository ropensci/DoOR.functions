mapReceptor <-
function(da, by.column, Receptors, ResponseMatrix) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# mapReceptor.R:
#################

# Mapping the given receptor into response matrix by comparing their linear relationship (Pearson Correlation Coefficient). 

# input parameters:
####################


# da 		 : data frame; odorant response data for a given receptor, e.g. Or22a. 
# by.column 	 : character string; specifying the column in "da" that contains the response values.
# Receptors 	 : character vector; containing the names of receptor that specify the consensus values in response matrix
# ResponseMatrix : a numeric matrix; containing the consensus response values.

# output is a numeric vector that contains the Pearson Correlation Coefficient between given data and selected consensus data in response matrix.
{
	res 		<- numeric()
	matchOdor 	<- match(da[,"CAS"], rownames(ResponseMatrix))
	for (i in Receptors)
	{
		dataVector 	<- da[,by.column]
		ResponseMatrixi <- ResponseMatrix[matchOdor,i]
		xy 		<- na.omit(cbind(dataVector,ResponseMatrixi))
		if(is.na(which(!is.na(ResponseMatrix[matchOdor,i]))[1]) | dim(xy)[1]==0)
		# if no data available for selected receptor or no overlapped values with the selected receptor, then return NA and run next loop
		{
			meanDist <- NA
			next
		}
		if (lm(ResponseMatrixi~dataVector)$coef[2]==0 | 
			is.na(lm(ResponseMatrixi~dataVector)$coef[2]))
		# if the two data are fitted horizontally or vertically, then return NA and run next loop
		{
			meanDist <- NA
			next
		}
		else 
		{
			corCoeff <- cor.test(x= dataVector,y=ResponseMatrixi)$estimate
		}
		Rss 		<- corCoeff
		names(Rss) 	<- i		# assign the receptor name to "corCoeff"
		res 		<- c(res,Rss)
	}
	return(res)
}
