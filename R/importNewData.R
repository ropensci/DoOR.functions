#' Import new data into DoOR
#'
#' Import or update new data and update \code{weight.globNorm},
#' \code{response.range}, \code{odor}, \code{ORs} and receptor data frames.
#'
#' \code{\link{importNewData}} is used to import new data into database. If the
#' data contains a new receptor or ORN, then build a new data frame for this
#' receptor or ORN. If the data contains a receptor that is already present in
#' database, then merge the imported data into old data frame. The information
#' (e.g. response range, how many receptors and odors were measured from given
#' study) will be integrated into data \code{response.range}, \code{odor},
#' \code{ORs} and \code{weight.globNorm}. If an existing study is imported,
#' \code{\link{removeStudy}} will be run first in order to perform an update.
#'
#' @param file.name character string, the name of given file that contains
#'   response values of one or more odorant receptors, either a .csv or .txt
#'   file.
#' @param dataFormat data frame, a data frame does not contain any response
#'   value but odorant information.
#' @param odor_data data frame, contains the odorant information.
#' @param weightGlobNorm data matrix, indicates whether given receptor has been
#'   measured by given study.
#' @param responseRange data frame, contains the information about response
#'   range of each study and how many odors have been measured in each study.
#' @param receptors data frame, contains the receptor and OSN names and their
#'   expression.
#' @param ident the identifier used for matching, usually the InChIKey is used.
#' @param round the number of digits the imported values are rounded to.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @export
#' @importFrom stats na.omit
#' @importFrom utils read.csv read.table
#' @keywords data
#' @examples
#' \dontrun{
#' import new data named "odorantResponses_Orx.txt" into database and update the support data.
#' library(DoOR.data)
#' importNewData(file.name = "odorantResponses_Orx.csv")
#' }
#'
importNewData <- function(file.name,
                          dataFormat = door_default_values("data.format"),
                          odor_data  = door_default_values("odor"),
                          weightGlobNorm = door_default_values("weight.globNorm"),
                          responseRange  = door_default_values("response.range"),
                          receptors = door_default_values("ORs"),
                          ident     = door_default_values("ident"),
                          round     = 3) {
  if (any(grep(".txt$", file.name))) {
    imported.data <- read.table(file.name)
    file.name <- gsub(".txt$", "", file.name)
  }
  if (any(grep(".csv$", file.name))) {
    imported.data <- read.csv(file.name)
    file.name <- gsub(".csv$", "", file.name)
  }

  if(!("Name" %in% colnames(imported.data)) | !("InChIKey" %in% colnames(imported.data)))
    stop("'InChIKey' or 'Name' column missing, these two are mandatory!")

  # check for already existing studies
  if (any(file.name %in% responseRange$study)) {
    existing <- which(file.name %in% responseRange$study)
    existing <- file.name[existing]
    for(i in existing)
      removeStudy(i)

    # get updated versions from .globalEnv
    responseRange <- response.range
    weightGlobNorm <- weight.globNorm
    warning(paste('The following studies were already existing and were removed prior to integrating the new data:\n', paste(existing, collapse = ', ')))
  }

  # check for missing identifiers
  if(any(is.na(imported.data[,ident]))) {
    nas <- which(is.na(imported.data[,ident]))
    imported.data <- droplevels(imported.data[-nas,])
    warning('We found ', length(nas), ' missing identifier(s), this data was removed!', call. = FALSE)
  }

  # check for duplicated identifiers
  if(any(duplicated(tolower(imported.data[,ident]))))
    stop('There are duplicated identifiers in the new dataset, please solve this first.')

  # convert CID to character
  whichCIDCol <- grep("CID",names(imported.data))
  if (!is.na(whichCIDCol[1])) {
    imported.data[,whichCIDCol] <- factor(imported.data[,whichCIDCol])
  }

  # look for columns containing numerical values (response data)
  nv 	<- as.numeric(which(sapply(imported.data, is.numeric)))
  n 	<- length(nv)
  receptor_file <- colnames(imported.data)[nv]

  if (round != FALSE) {
    imported.data[nv] <- round(imported.data[nv],round)
  }

  # update data matrix "weight.globNorm"
  dim_weightGlobNorm <- dim(weightGlobNorm)
  weightGlobNorm[,dim_weightGlobNorm[2]+1] <- NA
  colnames(weightGlobNorm)[dim_weightGlobNorm[2]+1] <- file.name

  matchreceptor <- match(receptor_file,rownames(weightGlobNorm))
  stats <- list('importedReceptors' = length(receptor_file), 'newReceptors' = 0, 'updatedOdors' = 0, 'newOdors' = 0)
  if (any(is.na(matchreceptor))) {
    whichNotmatch 		<- which(is.na(matchreceptor))
    newReceptor 		<- receptor_file[whichNotmatch]
    dim_weightGlobNorm 	<- dim(weightGlobNorm)
    lastRow 		<- (dim_weightGlobNorm[1]+length(whichNotmatch))
    seqlastRows 		<- ((dim_weightGlobNorm[1]+1):lastRow)
    weightGlobNorm[seqlastRows,dim_weightGlobNorm[2]] <- NA
    rownames(weightGlobNorm)[seqlastRows] <- newReceptor
    message(paste(newReceptor, "has been added into 'weight.globNorm'.", collapse = '\n'))
  }

  # update data frame "response range"
  responseRange_new.file <- range(imported.data[,nv], na.rm=TRUE)
  responseRange_new      <- data.frame(study   = file.name,
                                       min 	  = responseRange_new.file[1],
                                       max  	  = responseRange_new.file[2],
                                       n_odors = sum(apply(!is.na(imported.data[nv]), 1, sum) > 0))# dim(imported.data)[1]) # changed as the old way also returned NAs

  responseRange <- rbind(responseRange, responseRange_new)

  # update data frame "odor" and data.format if new odor is available.
  matchtoOdor <- match(imported.data[,ident], odor_data[,ident])
  whichNA     <- which(is.na(matchtoOdor))

  stats$updatedOdors <- length(na.omit(matchtoOdor))
  stats$newOdors     <- length(whichNA)

  if (!identical(odor_data[1:5], dataFormat))
    stop("The odorant lists of data 'odor' and 'data.format' are not identical. Please check them again.")
  if (is.na(whichNA[1])) {
    message("There were no new odors imported.")
  } else {
    ##########
    # add new odor identifiers to 'odor' and 'data.format' --------------------
    ##########
    newOdor <- as.character(imported.data[whichNA, "Name"])
    message(paste(newOdor, " - is a new odor. Data frames 'odor' and 'data.format' will be updated.", collapse='\n'))

    dim_odor 	<- dim(odor_data)

    odor_data[(dim_odor[1]+length(whichNA)),] <- NA

    levels(odor_data$InChIKey) <- union(levels(odor_data$InChIKey),levels(imported.data$InChIKey))
    odor_data[(dim_odor[1]+(1:length(whichNA))),"InChIKey"]       <-   as.character(imported.data[whichNA,"InChIKey"])

    levels(odor_data$Name) <- union(levels(odor_data$Name),levels(imported.data$Name))
    odor_data[(dim_odor[1]+(1:length(whichNA))),"Name"]    	      <-   as.character(imported.data[whichNA,"Name"])

    if ('CAS' %in% colnames(imported.data)) {
      levels(odor_data$CAS) <- union(levels(odor_data$CAS),levels(imported.data$CAS))
      odor_data[(dim_odor[1]+(1:length(whichNA))),"CAS"]    	<- as.character(imported.data[whichNA,"CAS"])
    }

    if ('CID' %in% colnames(imported.data)) {
      levels(odor_data$CID) <- union(levels(odor_data$CID),levels(imported.data$CID))
      odor_data[(dim_odor[1]+(1:length(whichNA))),"CID"]    	      <-   as.character(imported.data[whichNA,"CID"])
    }

    if ('Class' %in% colnames(imported.data)) {
      levels(odor_data$Class) <- union(levels(odor_data$Class),levels(imported.data$Class))
      odor_data[(dim_odor[1]+(1:length(whichNA))),"Class"]    	    <-   as.character(imported.data[whichNA,"Class"])
    }

    if ('InChI' %in% colnames(imported.data)) {
      levels(odor_data$InChI) <- union(levels(odor_data$InChI),levels(imported.data$InChI))
      odor_data[(dim_odor[1]+(1:length(whichNA))),"InChI"]          <-   as.character(imported.data[whichNA,"InChI"])
    }

    if ('SMILES' %in% colnames(imported.data)) {
      levels(odor_data$SMILES) <- union(levels(odor_data$SMILES),levels(imported.data$SMILES))
      odor_data[(dim_odor[1]+(1:length(whichNA))),"SMILES"]       <-   as.character(imported.data[whichNA,"SMILES"])
    }

    dataFormat <- odor_data[1:5]

    message("If data was provided, 'CAS', 'NAME', 'CID', 'Class', 'InChI' and 'SMILES' columns were updated.")
  }

  ##########
  # if there is a new receptor or ORN update data frame "ORs", NOTE: the AL projection pattern (DoOR.mappings) should be added manually
  ##########

  match_receptor <- match(receptor_file, receptors[,"OR"])
  what_is_new <-  receptor_file[which(is.na(match_receptor))]
  if (!is.na(what_is_new[1])) {
    ORs_new 	<- data.frame(OR = what_is_new, expression = NA)
    receptors 	<- rbind(receptors, ORs_new)
    message("New receptor or ORN has been added to 'ORs', please input the AL mapping manually.")
  }

  ##########
  # add new response data
  ##########

  for (j in receptors[,"OR"]) {
    target <- try(get(j),silent=TRUE)			    # try to get the data and assign it a names "target"
    if (inherits(target, "try-error")) {			# if it can be done, there must be a new receptor
      target <- dataFormat
      message(paste(j, "is a new receptor or ORN. A new response data.frame was created."))
    }

    matchOdor <- match(imported.data[,ident],target[,ident])
    whichNA 	<- which(is.na(matchOdor))
    if (is.na(whichNA[1])) {
      assign(j, target, envir = .GlobalEnv)
    } else {
      dim_RD 	<- dim(target)
      target[(dim_RD[1]+length(whichNA)),] 	<- NA

      levels(target$InChIKey) <- union(levels(target$InChIKey),levels(imported.data$InChIKey))
      target[(dim_RD[1]+(1:length(whichNA))),"InChIKey"] <- as.character(imported.data[whichNA,"InChIKey"])

      levels(target$Name) <- union(levels(target$Name),levels(imported.data$Name))
      target[(dim_odor[1]+(1:length(whichNA))),"Name"] <- as.character(imported.data[whichNA,"Name"])

      if ('Class' %in% colnames(imported.data)) {
        levels(target$Class) <- union(levels(target$Class),levels(imported.data$Class))
        target[(dim_RD[1]+(1:length(whichNA))),"Class"] <- as.character(imported.data[whichNA,"Class"])
      }

      if ('CAS' %in% colnames(imported.data)) {
        levels(target$CAS) <- union(levels(target$CAS),levels(imported.data$CAS))
        target[(dim_RD[1]+(1:length(whichNA))),"CAS"] <- as.character(imported.data[whichNA,"CAS"])
      }

      if ('CID' %in% colnames(imported.data)) {
        levels(target$CID) <- union(levels(target$CID),levels(imported.data$CID))
        target[(dim_odor[1]+(1:length(whichNA))),"CID"] <- as.character(imported.data[whichNA,"CID"])
      }

      assign(j, target, envir = .GlobalEnv) 				# assign the target back to his real name
    }
  }

  # import data
  for (i in 1:n) {
    column.name <- receptor_file[i] 			        # receptor name
    target <- try(get(column.name),silent=TRUE) 	# try to find a match receptor and load data from old database

    # check if receptor is new
    if(dim(target)[2] == 5)
      stats$newReceptors <- stats$newReceptors + 1

    assign(column.name,
           combine_data(data1 = target,
                    data2 = imported.data,
                    by.data2 = column.name,
                    assigned.name = paste(file.name, sep="")),
           envir = .GlobalEnv)
    message(paste(column.name,"has been imported."))

    # update weight.globNorm
    dim_weightGlobNorm 	<- dim(weightGlobNorm)
    match_receptor 		  <- match(column.name, rownames(weightGlobNorm))
    weightGlobNorm[match_receptor,dim_weightGlobNorm[2]] <- 1
  } # END for (i in 1:n)

  # return the updates back to working enviroment
  assign("data.format", dataFormat, envir = .GlobalEnv)
  assign("weight.globNorm", weightGlobNorm, envir = .GlobalEnv)
  assign("response.range", responseRange, envir = .GlobalEnv)
  assign("ORs", receptors, envir = .GlobalEnv)
  assign("odor", odor_data, envir = .GlobalEnv)

  message()
  message(paste('###################\n',
                'Import statistics:\n',
                stats$importedReceptors - stats$newReceptors,'response profiles were updated,',stats$newReceptors,'new profiles were added to DoOR.\n',
                stats$updatedOdors,'odorants were updated,',stats$newOdors,'new odorants were added to DoOR.'))

} # END program "importNewData"
