importNewData <-
function(file.name, file.format, dataFormat = default.val("data.format"),
			odor.data = default.val("odor.data"), 
			weightGlobNorm = default.val("weight.globNorm"), 
			responseRange = default.val("response.range"), 
			receptors = default.val("ORs") )

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany



# importnewdata.r:
###################

## import new data and update the weights (weight.globNorm), response range (response.range) and receptor names (ors)


# input parameters:
#####################

# file.name 	 : character, the name of given file that contains response values of one or more odorant receptors
# file.format 	 : character; either ".txt" or ".csv"
# dataFormat 	 : data frame, a data frame does not contain any response value but odorant information.
# odor.data 	 : data frame, containing the odorant information
# weightGlobNorm : data matrix, indicates whether given receptor has been measured by given study.
# responseRange  : data frame, contains the information about response range of each study and how many odors have been measured in each study.
# receptors 	 : data frame, contains the receptor and ORN names and their expression.


#output: the updated weights, etc. are written to the workspace

{
	loadRD()	# load all available response data

	if (file.format == ".txt") { imported_data <- read.table( paste(file.name,file.format,sep="") ) }
	if (file.format == ".csv") { imported_data <- read.csv( paste(file.name,file.format,sep="") ) }
	
	whichCIDCol <- grep("CID",names(imported_data))
	if (!is.na(whichCIDCol[1]))
	{
		imported_data[,whichCIDCol] <- as.character(imported_data[,whichCIDCol])
	}
	nv 	<- as.numeric(which(sapply(imported_data, is.numeric)))
	n 	<- length(nv)
	receptor_file <- colnames(imported_data)[nv]

	# update data matrix "weight.globNorm"
	dim_weightGlobNorm <- dim(weightGlobNorm)
	weightGlobNorm[,dim_weightGlobNorm[2]+1] <- NA
	colnames(weightGlobNorm)[dim_weightGlobNorm[2]+1] <- file.name

	matchreceptor <- match(receptor_file,rownames(weightGlobNorm))
	if (any(is.na(matchreceptor)))
	{
		whichNotmatch 		<- which(is.na(matchreceptor))
		newReceptor 		<- receptor_file[whichNotmatch]
		dim_weightGlobNorm 	<- dim(weightGlobNorm)
		lastRow 		<- (dim_weightGlobNorm[1]+length(whichNotmatch))
		seqlastRows 		<- ((dim_weightGlobNorm[1]+1):lastRow)
		weightGlobNorm[seqlastRows,dim_weightGlobNorm[2]] <- NA
		rownames(weightGlobNorm)[seqlastRows] <- newReceptor
		print( paste(newReceptor, "has been added into 'weight.globNorm'.") )	
	}
 
	# update data frame "response range"
	responseRange_new.file 	<- range(imported_data[,nv],na.rm=TRUE)
	responseRange_new 	<- data.frame( study   = file.name,
					  min 	  = responseRange_new.file[1],
					  max  	  = responseRange_new.file[2],
					  n_odors = dim(imported_data)[1])

	responseRange 		<- rbind(responseRange, responseRange_new)

	# update data frame "odor" and data.format if new odor is available.
	matchtoOdor 	<- match(imported_data[,"CAS"],odor.data$CAS)
	matchtoDF 	<- match(imported_data[,"CAS"],dataFormat$CAS)

	if (any(is.na(match(matchtoOdor,matchtoDF)))) 
	{ 
		stop("The odorant lists of data 'odor' and 'data.format' are not identical. Please check them again.") 
	}
	else 
	{
		whichNA 	<- which(is.na(matchtoOdor))
		newOdor 	<- as.character(imported_data[whichNA,"CAS"])

		print( paste(newOdor, "is a new odor. Data frames 'odor' and 'data.format' will be updated.") )

		dim_odor 	<- dim(odor.data)
		dim_data.format <- dim(dataFormat)

		odor.data[(dim_odor[1]+length(whichNA)),] 		<- NA
		dataFormat[(dim_data.format[1]+length(whichNA)),] 	<- NA
		levels(odor.data$CAS) <- c(levels(odor.data$CAS),newOdor)
		levels(dataFormat$CAS) <- c(levels(dataFormat$CAS),newOdor)
		odor.data[(dim_odor[1]+(1:length(whichNA))),"CAS"]  		<- newOdor
		dataFormat[(dim_data.format[1]+(1:length(whichNA))),"CAS"]  	<- newOdor
		message( "Only 'CAS' column of data has been updated." )

		# update response data for each receptor, if new odor is available.
	}

	# if there is new receptor or ORN updata data frame "ORs"; NOTE: the expression should be added manually

	match_receptor <- match(receptor_file, receptors[,"OR"])
	what_is_new <-  receptor_file[which(is.na(match_receptor))]
	if (!is.na(what_is_new[1])) 
	{ 
		ORs_new 	<- data.frame(OR = what_is_new, expression = NA) 
		receptors 	<- rbind(receptors, ORs_new)

		message( "New receptor or ORN has been added in 'ORs', please input the expression manually." )
	}
	
	for (j in receptors[,"OR"])
	{
		target 		<- try(get(j),silent=TRUE)			# try to get the data and assign it a names "target"
		if (inherits(target, "try-error"))				# if it can be done, there must be a new receptor
		{
		target 		<- dataFormat
		}
		matchOdor 	<- match(imported_data[,"CAS"],target$CAS)
		whichNA 	<- which(is.na(matchOdor))
		if (is.na(whichNA[1])) {
			assign(j, target, envir = .GlobalEnv) 
			print( paste(j, "is a new receptor or ORN. A new response data is builded.") )
		}
		else {
			dim_RD 	<- dim(target)
			target[(dim_RD[1]+length(whichNA)),] 	<- NA
			levels(target$CAS) <- c(levels(target$CAS),newOdor)
			target[(dim_RD[1]+(1:length(whichNA))),"CAS"]  	<- newOdor
			assign(j, target, envir = .GlobalEnv) 				# assign the target back to his real name
		}
	}
	
	# import data
	for (i in 1:n)
	{
		column.name 	<- receptor_file[i] 			# receptor name
		target 		<- try(get(column.name),silent=TRUE) 	# try to find a match receptor and load data from old database
		
		assign(column.name, 
			combData(data1 = target, data2 = imported_data, 
				by.data2 = column.name, 
				assigned.name = paste( file.name, sep="" )),envir = .GlobalEnv)

		# update weight.globNorm
		dim_weightGlobNorm 	<- dim(weightGlobNorm)
		match_receptor 		<- match(column.name, rownames(weightGlobNorm))
		weightGlobNorm[match_receptor,dim_weightGlobNorm[2]] <- 1
	} # END for (i in 1:n)

	# return the updates back to working enviroment
	assign("data.format", dataFormat, envir = .GlobalEnv)
	assign("weight.globNorm", weightGlobNorm, envir = .GlobalEnv)
	assign("response.range", responseRange, envir = .GlobalEnv)
	assign("ORs", receptors, envir = .GlobalEnv)
	assign("odor", odor.data, envir = .GlobalEnv)

}
