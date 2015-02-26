importNewData <-
function(file.name, file.format, dataFormat = default.val("data.format"),
			odor.data = default.val("odor.data"), 
			weightGlobNorm = default.val("weight.globNorm"), 
			responseRange = default.val("response.range"), 
			receptors = default.val("ORs") )

# part of the DoOR package: (c) 2015 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany  
  
# importnewdata.r:
###################

## import new data and update the weights (weight.globNorm), response range (response.range) and receptor names (ors)


# input parameters:
#####################

# file.name    : character, the name of given file that contains response values of one or more odorant receptors
# file.format 	 : character; either ".txt" or ".csv"
# dataFormat 	 : data frame, a data frame does not contain any response value but odorant information.
# odor.data 	 : data frame, containing the odorant information
# weightGlobNorm : data matrix, indicates whether given receptor has been measured by given study.
# responseRange  : data frame, contains the information about response range of each study and how many odors have been measured in each study.
# receptors 	 : data frame, contains the receptor and ORN names and their AL projection pattern.


#output: the updated weights, etc. are written to the workspace

# example: 
# importNewData(file.name="Daniel.2011.nmr", file.format=".txt", dataFormat  = data.format, weightGlobNorm  = weight.globNorm, responseRange = response.range, receptors = ORs,odor.data = odor)

{
  if (file.format == ".txt") { imported_data <- read.table( paste(file.name,file.format,sep="") ) }
  if (file.format == ".csv") { imported_data <- read.csv( paste(file.name,file.format,sep="") ) }
  
  whichCIDCol <- grep("CID",names(imported_data))
  if (!is.na(whichCIDCol[1]))
  {
    imported_data[,whichCIDCol] <- factor(imported_data[,whichCIDCol])
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
    message( paste(newReceptor, "has been added into 'weight.globNorm'.") )	
  }
  
  # update data frame "response range"
  responseRange_new.file 	<- range(imported_data[,nv],na.rm=TRUE)
  responseRange_new 	<- data.frame( study   = file.name,
                                    min 	  = responseRange_new.file[1],
                                    max  	  = responseRange_new.file[2],
                                    n_odors = sum(apply(!is.na(imported_data[nv]),1,sum) > 0) )# dim(imported_data)[1]) # changed as the old way also returned NAs
  
  responseRange 		<- rbind(responseRange, responseRange_new)
  
  # update data frame "odor" and data.format if new odor is available.
  matchtoOdor 	<- match(imported_data[,"CAS"],odor.data$CAS)
  matchtoDF 	<- match(imported_data[,"CAS"],dataFormat$CAS)
  whichNA 	<- which(is.na(matchtoOdor))
  
  if (any(is.na(match(matchtoOdor,matchtoDF)))) 
  { 
    stop("The odorant lists of data 'odor' and 'data.format' are not identical. Please check them again.") 
  }
  if (is.na(whichNA[1]))
  {
    message("There were no new odors imported.")
  }
  else 
  {
    newOdor 	<- as.character(imported_data[whichNA,"CAS"])
    
    message( paste(newOdor, "is a new odor. Data frames 'odor' and 'data.format' will be updated.") )
    
    dim_odor 	<- dim(odor.data)
    dim_data.format <- dim(dataFormat)
    
    odor.data[(dim_odor[1]+length(whichNA)),] 		<- NA
    dataFormat[(dim_data.format[1]+length(whichNA)),] 	<- NA
    
    levels(odor.data$CAS) <- c(levels(odor.data$CAS),newOdor)
    levels(dataFormat$CAS) <- c(levels(dataFormat$CAS),newOdor)
    odor.data[(dim_odor[1]+(1:length(whichNA))),"CAS"]  		<- newOdor
    dataFormat[(dim_data.format[1]+(1:length(whichNA))),"CAS"]  	<- newOdor
    
    levels(odor.data$Name) <- union(levels(odor.data$Name),levels(imported_data$Name))
    levels(dataFormat$Name) <- union(levels(dataFormat$Name),levels(imported_data$Name))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"Name"]    	      <-   as.character(imported_data[whichNA,"Name"])
    dataFormat[(dim_data.format[1]+(1:length(whichNA))),"Name"]  	<-   as.character(imported_data[whichNA,"Name"])  
    
    if ('CID' %in% colnames(imported_data)){
    levels(odor.data$CID) <- union(levels(odor.data$CID),levels(imported_data$CID))
    levels(dataFormat$CID) <- union(levels(dataFormat$CID),levels(imported_data$CID))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"CID"]    	      <-   as.character(imported_data[whichNA,"CID"])
    dataFormat[(dim_data.format[1]+(1:length(whichNA))),"CID"]  	<-   as.character(imported_data[whichNA,"CID"])
    }
    
    if ('Class' %in% colnames(imported_data)){
    levels(odor.data$Class) <- union(levels(odor.data$Class),levels(imported_data$Class))
    levels(dataFormat$Class) <- union(levels(dataFormat$Class),levels(imported_data$Class))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"Class"]    	    <-   as.character(imported_data[whichNA,"Class"])
    dataFormat[(dim_data.format[1]+(1:length(whichNA))),"Class"]  <-   as.character(imported_data[whichNA,"Class"]) 
    }
    
    if ('InChI' %in% colnames(imported_data)){
    levels(odor.data$InChI) <- union(levels(odor.data$InChI),levels(imported_data$InChI))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"InChI"]          <-   as.character(imported_data[whichNA,"InChI"])
    }
    
    if ('InChIKey' %in% colnames(imported_data)){
    levels(odor.data$InChIKey) <- union(levels(odor.data$InChIKey),levels(imported_data$InChIKey))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"InChIKey"]       <-   as.character(imported_data[whichNA,"InChIKey"])
    }
    
    if ('SMILES' %in% colnames(imported_data)){
    levels(odor.data$SMILES) <- union(levels(odor.data$SMILES),levels(imported_data$SMILES))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"SMILES"]       <-   as.character(imported_data[whichNA,"SMILES"])
    }
    
    
    
    
    
    message( "If data was provided, 'CAS', 'NAME', 'CID', 'Class', 'InChI' and 'SMILES' columns were updated." )
    
    # update response data for each receptor, if new odor is available.
  }
  
  # if there is new receptor or ORN update data frame "ORs"; NOTE: the AL projection pattern (OGN) should be added manually
  
  match_receptor <- match(receptor_file, receptors[,"OR"])
  what_is_new <-  receptor_file[which(is.na(match_receptor))]
  if (!is.na(what_is_new[1])) 
  { 
    ORs_new 	<- data.frame(OR = what_is_new, expression = NA) 
    receptors 	<- rbind(receptors, ORs_new)
    message( "New receptor or ORN has been added in 'ORs', please input the AL projection pattern manually." )
  }  
    
    # add new response data
    for (j in receptors[,"OR"])
    {
      target 		<- try(get(j),silent=TRUE)			# try to get the data and assign it a names "target"
      if (inherits(target, "try-error"))				# if it can be done, there must be a new receptor
      {
        target 		<- dataFormat
        message( paste(j, "is a new receptor or ORN. A new response data.frame was created.") )
      }
      matchOdor 	<- match(imported_data[,"CAS"],target$CAS)
      whichNA 	<- which(is.na(matchOdor))
      if (is.na(whichNA[1])) {
        assign(j, target, envir = .GlobalEnv) 
      }
      else {
        dim_RD 	<- dim(target)
        target[(dim_RD[1]+length(whichNA)),] 	<- NA
        
        levels(target$CAS) <- c(levels(target$CAS),newOdor)
        target[(dim_RD[1]+(1:length(whichNA))),"CAS"]  	<- newOdor
        
        levels(target$Name) <- union(levels(target$Name),levels(imported_data$Name))
        target[(dim_odor[1]+(1:length(whichNA))),"Name"] <- as.character(imported_data[whichNA,"Name"])
        
        if ('CID' %in% colnames(imported_data)){
        levels(target$CID) <- union(levels(target$CID),levels(imported_data$CID))
        target[(dim_odor[1]+(1:length(whichNA))),"CID"] <- as.character(imported_data[whichNA,"CID"])
        }
        
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
    message(paste(column.name,"has been imported."))
    
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
  
} # END program "importNewData"