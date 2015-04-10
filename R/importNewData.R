#' import new data into DoOR
#' 
#' import new data and update the weight, response range and receptor names
#' 
#' \code{\link{importNewData}} is used to import new data into database. If the
#' data contains a new receptor or ORN, then build a new data frame for this
#' receptor or ORN. If the data contains a receptor that is already present in
#' database, then merge the imported data into old data frame with function
#' \code{\link{combData}}. The information (e.g. response range, how many
#' receptors and odors were measured from given study) will be integrated into
#' data "response.range", "ORs" and "weight.globNorm".
#' 
#' @param file.name character string; the name of given file that contains
#' response values of one or more odorant receptors.
#' @param file.format character string; the format of given file, either ".txt"
#' or ".csv"
#' @param dataFormat data frame; a data frame does not contain any response
#' value but odorant information.
#' @param odor.data data frame; a data frame that contains the odorant
#' information.
#' @param weightGlobNorm data matrix; indicates whether given receptor has been
#' measured by given study.
#' @param responseRange data frame; contains the information about response
#' range of each study and how many odors have been measured in each study.
#' @param receptors data frame, contains the receptor and ORN names and their
#' expression.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @keywords data
#' @examples
#' 
#' # import new data named "odorantResponses_Orx.txt" into database and update the support data.
#' # library(DoOR.data)
#' # importNewData(file.name="odorantResponses_Orx", file.format=".txt")
#' 
importNewData <- function(file.name, file.format, dataFormat = default.val("data.format"),
                          odor.data = default.val("odor.data"), 
                          weightGlobNorm = default.val("weight.globNorm"), 
                          responseRange = default.val("response.range"), 
                          receptors = default.val("ORs"),
                          ident = default.val("ident"),
                          round = 3)
{
  if (file.format == ".txt") { imported_data <- read.table(paste(file.name,file.format,sep="")) }
  if (file.format == ".csv") { imported_data <- read.csv(paste(file.name,file.format,sep="")) }
  
  if(any(duplicated(tolower(imported_data[,ident])))) stop('There are duplicated identifiers in the new dataset, please solve this first.')
  
  if(any(is.na(imported_data[,ident]))) 
  {
    nas <- which(is.na(imported_data[,ident]))
    imported_data <- droplevels(imported_data[-nas,])
    warning('We found ', length(nas), ' misisng identifier(s), this data was removed!')
  }
 
  whichCIDCol <- grep("CID",names(imported_data))
  if (!is.na(whichCIDCol[1]))
  {
    imported_data[,whichCIDCol] <- factor(imported_data[,whichCIDCol])
  }
  nv 	<- as.numeric(which(sapply(imported_data, is.numeric)))
  n 	<- length(nv)
  receptor_file <- colnames(imported_data)[nv]
  
  if (round != F) 
  {
    imported_data[nv] <- round(imported_data[nv],round)
  }
  
  # update data matrix "weight.globNorm"
  dim_weightGlobNorm <- dim(weightGlobNorm)
  weightGlobNorm[,dim_weightGlobNorm[2]+1] <- NA
  colnames(weightGlobNorm)[dim_weightGlobNorm[2]+1] <- file.name
  
  matchreceptor <- match(receptor_file,rownames(weightGlobNorm))
  stats <- list('updatedReceptors' = length(na.omit(matchreceptor)), 'newReceptors' = 0, 'updatedOdors' = 0, 'newOdors' = 0)
  if (any(is.na(matchreceptor)))
  {
    whichNotmatch 		<- which(is.na(matchreceptor))
    newReceptor 		<- receptor_file[whichNotmatch]
    dim_weightGlobNorm 	<- dim(weightGlobNorm)
    lastRow 		<- (dim_weightGlobNorm[1]+length(whichNotmatch))
    seqlastRows 		<- ((dim_weightGlobNorm[1]+1):lastRow)
    weightGlobNorm[seqlastRows,dim_weightGlobNorm[2]] <- NA
    rownames(weightGlobNorm)[seqlastRows] <- newReceptor
    message(paste(newReceptor, "has been added into 'weight.globNorm'.", collapse = '\n'))
    stats$newReceptors <- length(newReceptor)
  }
  
  # update data frame "response range"
  responseRange_new.file 	<- range(imported_data[,nv],na.rm=TRUE)
  responseRange_new     	<- data.frame(study   = file.name,
                                       min 	  = responseRange_new.file[1],
                                       max  	  = responseRange_new.file[2],
                                       n_odors = sum(apply(!is.na(imported_data[nv]),1,sum) > 0))# dim(imported_data)[1]) # changed as the old way also returned NAs
  
  responseRange <- rbind(responseRange, responseRange_new)
  
  # update data frame "odor" and data.format if new odor is available.
  matchtoOdor <- match(imported_data[,ident],odor.data[,ident])
  whichNA     <- which(is.na(matchtoOdor))
  
  stats$updatedOdors <- length(na.omit(matchtoOdor))
  stats$newOdors     <- length(whichNA) 
  
  if (!identical(odor.data[1:5],dataFormat)) stop("The odorant lists of data 'odor' and 'data.format' are not identical. Please check them again.") 
  if (is.na(whichNA[1]))
  {
    message("There were no new odors imported.")
  }
  else 
  {
    ##########
    # add new odor identifiers to 'odor' and 'data.format' -------------------- 
    ##########
    newOdor 	<- as.character(imported_data[whichNA,"Name"])
    message(paste(newOdor, '\n', "is a new odor. Data frames 'odor' and 'data.format' will be updated.", collapse='\n'))
    
    dim_odor 	<- dim(odor.data)
    
    odor.data[(dim_odor[1]+length(whichNA)),] 		<- NA
    
    levels(odor.data$InChIKey) <- union(levels(odor.data$InChIKey),levels(imported_data$InChIKey))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"InChIKey"]       <-   as.character(imported_data[whichNA,"InChIKey"])
    
    levels(odor.data$Name) <- union(levels(odor.data$Name),levels(imported_data$Name))
    odor.data[(dim_odor[1]+(1:length(whichNA))),"Name"]    	      <-   as.character(imported_data[whichNA,"Name"])
    
    if ('CAS' %in% colnames(imported_data)){
      levels(odor.data$CAS) <- union(levels(odor.data$CAS),levels(imported_data$CAS))
      odor.data[(dim_odor[1]+(1:length(whichNA))),"CAS"]    	<- as.character(imported_data[whichNA,"CAS"])
    }
    
    if ('CID' %in% colnames(imported_data)){
      levels(odor.data$CID) <- union(levels(odor.data$CID),levels(imported_data$CID))
      odor.data[(dim_odor[1]+(1:length(whichNA))),"CID"]    	      <-   as.character(imported_data[whichNA,"CID"])
    }
    
    if ('Class' %in% colnames(imported_data)){
      levels(odor.data$Class) <- union(levels(odor.data$Class),levels(imported_data$Class))
      odor.data[(dim_odor[1]+(1:length(whichNA))),"Class"]    	    <-   as.character(imported_data[whichNA,"Class"])
    }
    
    if ('InChI' %in% colnames(imported_data)){
      levels(odor.data$InChI) <- union(levels(odor.data$InChI),levels(imported_data$InChI))
      odor.data[(dim_odor[1]+(1:length(whichNA))),"InChI"]          <-   as.character(imported_data[whichNA,"InChI"])
    }
    
    if ('SMILES' %in% colnames(imported_data)){
      levels(odor.data$SMILES) <- union(levels(odor.data$SMILES),levels(imported_data$SMILES))
      odor.data[(dim_odor[1]+(1:length(whichNA))),"SMILES"]       <-   as.character(imported_data[whichNA,"SMILES"])
    }
    
    dataFormat <- odor.data[1:5]
    
    message("If data was provided, 'CAS', 'NAME', 'CID', 'Class', 'InChI' and 'SMILES' columns were updated.")
  }
  
  ##########
  # if there is a new receptor or ORN update data frame "ORs"; NOTE: the AL projection pattern (OGN) should be added manually
  ##########
  
  match_receptor <- match(receptor_file, receptors[,"OR"])
  what_is_new <-  receptor_file[which(is.na(match_receptor))]
  if (!is.na(what_is_new[1])) 
  { 
    ORs_new 	<- data.frame(OR = what_is_new, expression = NA) 
    receptors 	<- rbind(receptors, ORs_new)
    message("New receptor or ORN has been added to 'ORs', please input the AL mapping manually.")
  }  
  
  ##########
  # add new response data
  ##########
  
  for (j in receptors[,"OR"])
  {
    target <- try(get(j),silent=TRUE)			# try to get the data and assign it a names "target"
    if (inherits(target, "try-error"))				# if it can be done, there must be a new receptor
    {
      target <- dataFormat
      message(paste(j, "is a new receptor or ORN. A new response data.frame was created."))
    }
    
    matchOdor <- match(imported_data[,ident],target[,ident])
    whichNA 	<- which(is.na(matchOdor))
    if (is.na(whichNA[1])) {
      assign(j, target, envir = .GlobalEnv) 
    }
    else {
      dim_RD 	<- dim(target)
      target[(dim_RD[1]+length(whichNA)),] 	<- NA
      
      levels(target$InChIKey) <- union(levels(target$InChIKey),levels(imported_data$InChIKey))
      target[(dim_RD[1]+(1:length(whichNA))),"InChIKey"] <- as.character(imported_data[whichNA,"InChIKey"])
      
      levels(target$Name) <- union(levels(target$Name),levels(imported_data$Name))
      target[(dim_odor[1]+(1:length(whichNA))),"Name"] <- as.character(imported_data[whichNA,"Name"])
      
      if ('Class' %in% colnames(imported_data)){
        levels(target$Class) <- union(levels(target$Class),levels(imported_data$Class))
        target[(dim_RD[1]+(1:length(whichNA))),"Class"] <- as.character(imported_data[whichNA,"Class"])
      }
      
      if ('CAS' %in% colnames(imported_data)){
        levels(target$CAS) <- union(levels(target$CAS),levels(imported_data$CAS))
        target[(dim_RD[1]+(1:length(whichNA))),"CAS"] <- as.character(imported_data[whichNA,"CAS"])
      }
      
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
    column.name <- receptor_file[i] 			        # receptor name
    target <- try(get(column.name),silent=TRUE) 	# try to find a match receptor and load data from old database
    assign(column.name, 
           combData(data1 = target, data2 = imported_data, by.data2 = column.name, assigned.name = paste(file.name, sep="")),
           envir = .GlobalEnv)
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

  message()
  message(paste('###################\n',
                'Import statistics:\n',
                stats$updatedReceptors,'response profiles were updated,',stats$newReceptors,'new profiles were added to DoOR.\n',
                stats$updatedOdors,'odorants were updated,',stats$newOdors,'new odorants were added to DoOR.'))
  
} # END program "importNewData"
