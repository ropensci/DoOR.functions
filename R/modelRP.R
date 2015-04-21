#' Generates a model response
#' 
#' Runs the DoOR algorithm, that merges all measurements for one receptor into
#' a common response model.
#' 
#' Merging a data is processed by following: \enumerate{ \item Normalize all
#' response data in value [0,1].  \item Compute the correlation between studies
#' and selected the best pair using \code{\link{selectModel}}.  \item Merge the
#' first pair using function \code{\link{projectPoints}}.  \item Add other
#' datasets if the correlation between the growing model response and the new
#' dataset is below the correlation threshold (select.MDValue). Datasets
#' excluded based on this criterion will be appended in a separate list.  }
#' 
#' @param da data frame; a selected receptor containing measured responses from
#' studies.
#' @param select.MDValue numeric; threshold on the MD for rejecting a fit.
#' @param overlapValues numeric; a criterion using to refuse a data set that
#' has not enough overlap value.
#' @param responseRange data frame; contains response ranges for all studies.
#' @param weightGlobNorm data frame; a binary data matrix; 1 indicates given
#' odor has been measured in given study, NA indicates NOT.
#' @param glob.normalization logical; default is \code{TRUE}; performs a global
#' normalization for the model response. Otherwise (\code{FALSE}) response
#' values will be given in value from 0 to 1.
#' @param plot logical; If \code{FALSE}, plotting is suppressed. Default is
#' \code{FALSE}.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' data(Or35a)
#' data(weight.globNorm)
#' data(response.range)
#' # merge data without R2 selection
#' RP.Or35a <- modelRP(Or35a, plot = TRUE)
#' 
modelRP <- function(da, 
                    select.MDValue = default.val("select.MDValue"), 
                		overlapValues = default.val("overlapValues"),
                		responseRange = default.val("response.range"),
                		weightGlobNorm = default.val("weight.globNorm"), 
                		glob.normalization = default.val("glob.normalization"), 
                		plot = default.val("plot") ) {

  #safety check for input data format
  if (!is.data.frame(da)) {
    stop("Not a data frame. Stopped in modelRP.R")
  }

  # positions of columns that contain odor response vectors
  # should be columns 6...end (1..5 contain odor class, odor name, InChIKey, CID and CAS)
  # 
  nv 	 		          <- as.numeric(c((default.val("num.charColumns")+1):dim(da)[2]))
  number_of_studies <- length(nv)
  excluded 		      <- character()

  # if there is no study --> stop
  if (number_of_studies == 0) {
      stop("No study to treat. Stopped in modelRP.R")
  }


  # if there is only one study --> just normalise data and return
  if (number_of_studies == 1) {
      pda 	    <- apply(as.data.frame(da[, nv]), 2, DoORnorm)	# pda : processing data
      merged_data <- as.vector(pda)
      message("No merging performed as there is only one study in the dataset.")
      # merged_data thus corresponds to the only measured data set
# merged_data <- pda
  }


  # merge data sets only if there are two or more datasets
  # start by merging the data pair that matches best (criterion: minimal MD value)
  # 
  if (number_of_studies > 1) {
  # setup the page parameters to fit all graphs onto one page
  # based on the number of studies present
    if (plot == TRUE)  {
 	    lenNV_1  <- number_of_studies - 1
      nframe.X <- ceiling(lenNV_1^0.5)
     	nframe.Y <- ceiling(lenNV_1/nframe.X)
      op       <- par(mfrow = c((nframe.Y), (nframe.X)))
  	}
  	# end plot

  	# extract response data without information columns into pda
    pda <- apply(as.data.frame(da[, nv]), 2, DoORnorm) # processing data
  	# pda contains the columns (receptors) of the data matrix 
  	# the function DoORnorm has normalized each to [0,1]

    ###
  	# --> start the merging process: find the first pair 

  	candidate.studies <- colnames(pda)
  	# candidate.studies contains a list of all studies in this receptor
  	selected 	  <- selectModel(candidate = candidate.studies, merged_data = NULL, overlapValues, data_candidate = pda, merged = FALSE )
  	# selected contains the pair of studies with the best match
  	# including all parameters for the best match

  	if (names(selected$best.model) == "no.fitted.model")
	    stop("Can not find any fitted model to merge given data. Stopped in modelRP.R")

  	if (selected[[1]][[1]]$MD > select.MDValue)
  	  stop("Can not find any fitted model with MD value under given threshold criterion. Stopped in modelRP.R")

    # merge these two datasets to one model response.
  	# done by projecting response values onto the chosen function (and, if plot==TRUE, also plot this)
  	# use the function (best.model) found with selectModel above

   projected <- projectPoints(x = pda[,selected$selected.x], y = pda[,selected$selected.y], best.model = selected$best.model, xlab = selected$selected.x,    ylab = selected$selected.y, plot = plot, title = plot)

  	# the output of projectPoints is a list with odor responses, either observed in both studies, or only in one study.
  	# to integrate it into pda, change data type: list to vector
  	merged_data                                   <- rep(NA, length = dim(pda)[1])
 	  merged_data[projected$Double.Observations$ID] <- projected$Double.Observations$NDR
  	merged_data[projected$Single.Observation$ID]  <- projected$Single.Observation$NDR

  	#remove the merged studies from the data matrix pda
  	rest_data <- colnames(pda)[-match(c(selected$selected.y,selected$selected.x),colnames(pda))]

  	# merge the remaining studies 
  	# always merge into the model response
  	# while there are still studies to process: find the study to be merged next 
  	# (again, use calModel() to try all studies and fitting functions --> MD values)

  	ind <- length(rest_data) # how many studies are left to merge

  	while (ind > 0) {
  	  # find the best match
  		selected.next <- selectModel(candidate = rest_data, data_candidate = pda, overlapValues, merged_data = merged_data, merged = TRUE )
    	# if a threshold for the MD is given, stop when no study reaches this threshold criterion any more
      if ( (names(selected.next$best.model) == "initial") | (names(selected.next$best.model) == "no.fitted.model") | (selected.next$best.model[[1]]$MD > select.MDValue) )	{
        excluded  <- rest_data
    	  message("Not all datasets could be merged because they did not reach the criterion.")
	       ind <- 0
    	   next
      }
	
      # best match has been found, now merge it into the model response "merged_data"
      # done by projecting response values onto the chosen function (and, if plot==TRUE, also plot this)
      # use the function (best.model) found with selectModel above

  		projected <- projectPoints(x = pda[,selected.next$selected.x], y = merged_data, best.model = selected.next$best.model, xlab = selected.next$selected.x,    ylab = selected.next$selected.y,    plot = plot, title = plot)

     	# overwrite merged_data with new values 
  		merged_data                                     <- rep(NA, length = dim(pda)[1])
    	merged_data[projected$Double.Observations$ID]   <- projected$Double.Observations$NDR
	    merged_data[projected$Single.Observation$ID]    <- projected$Single.Observation$NDR
      #remove the merged study from the data matrix pda
 		  rest_data <- rest_data[-match(selected.next$selected.x,rest_data)]
	    ind <- ind - 1
  	} # while (ind > 0) 
  
  	# now all studies have been merged into model response (merged_data)
  } # end if (number_of_studies > 1) 



  # global normalization 
  if (glob.normalization == TRUE) {
    name.Stud    <- colnames(da)[nv]
    mp_orx       <- match(colnames(da)[nv], responseRange[,"study"])
    Rmax         <- apply(as.matrix(da[,nv]),2,function(x) max(range(x,na.rm=TRUE)))
    Smax         <- responseRange[mp_orx,"max"]
    merged_data  <- globalNorm(RMAX = Rmax,SMAX = Smax, MV = merged_data,
		name.Stud = name.Stud, weightGlobNorm = weightGlobNorm, responseRange = responseRange )
  }

  resd <- cbind(da[, c("Class", "Name", "InChIKey", "CID", "CAS")], merged_data)
  source.data = colnames(da)[nv][-match(excluded,colnames(da)[nv])]
 
return(list(Source.data = source.data, model.response = resd, Excluded.Data = excluded))
}
