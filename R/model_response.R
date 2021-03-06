#' Generates a model response
#'
#' Runs the DoOR algorithm, that merges all measurements for one receptor into
#' a common response model.
#'
#' Merging a data is processed by following: \enumerate{ \item Normalize all
#' response data in value [0,1].  \item Compute the correlation between studies
#' and selected the best pair using \code{\link{select_model}}.  \item Merge the
#' first pair using function \code{\link{project_points}}.  \item Add other
#' datasets if the correlation between the growing model response and the new
#' dataset is below the correlation threshold (select.MDValue). Datasets
#' excluded based on this criterion will be appended in a separate list.  }
#'
#' @param da data frame, a selected receptor containing measured responses from
#' studies.
#' @param select.MDValue numeric, threshold on the MD for rejecting a fit.
#' @param overlapValues numeric, a criterion using to refuse a data set that
#' has not enough overlap value.
#' @param responseRange data frame, contains response ranges for all studies.
#' @param weightGlobNorm data frame, a binary data matrix, 1 indicates given
#' odor has been measured in given study, NA indicates NOT.
#' @param glob.normalization logical, default is \code{TRUE}, performs a global
#' normalization for the model response. Otherwise (\code{FALSE}) response
#' values will be given in value from 0 to 1.
#' @param plot logical, If \code{FALSE}, plotting is suppressed. Default is
#' \code{FALSE}.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @aliases modelRP model_response
#' @export
#' @importFrom stats na.omit
#' @importFrom graphics par points
#' @examples
#' # load data
#' library(DoOR.data)
#' data(Or35a)
#' data(door_global_normalization_weights)
#' data(door_response_range)
#' 
#' # merge all existing data sets for Or35a into a consensus model response
#' model_response_Or35a <- model_response(Or35a, plot = TRUE)
#'
model_response <- function(da,
     select.MDValue = door_default_values("select.MDValue"),
     overlapValues = door_default_values("overlapValues"),
     responseRange = door_default_values("door_response_range"),
     weightGlobNorm = door_default_values("door_global_normalization_weights"),
     glob.normalization = door_default_values("glob.normalization"),
     plot = door_default_values("plot")) {
  #safety check for input data format
  if (!is.data.frame(da)) {
    stop("Not a data frame. Stopped in model_response.R")
  }
  
  da <- filter_data(da, overlapValues = overlapValues)
  excluded <- as.character(da$excluded$study)
  da <- da$data
  
  
  # positions of columns that contain odor response vectors should be columns
  # 6...end (1..5 contain odor class, odor name, InChIKey, CID and CAS)
  # 
  if (length(da) > door_default_values("num.charColumns")) {
    nv                 <-
      as.numeric(c((
        door_default_values("num.charColumns") + 1
      ):dim(da)[2]))
    number_of_studies  <- length(nv)
  } else {
    number_of_studies  <- 0
  }
  
  
  # if there is no study --> stop
  if (number_of_studies == 0) {
    merged_data <- rep(NA, dim(da)[1])
    glob.normalization <- FALSE
    nv <- 0
    message("No study to treat, returning NAs")
  }
  
  
  # if there is only one study --> just normalise data and return
  if (number_of_studies == 1) {
    pda 	    <-
      apply(as.data.frame(da[, nv]), 2, door_norm)	# pda : processing data
    merged_data <- as.vector(pda)
    message("No merging performed as there is only one study in the dataset.")
    # merged_data thus corresponds to the only measured data set
    # merged_data <- pda
  }
  
  
  # merge data sets only if there are two or more datasets start by merging the
  # data pair that matches best (criterion: minimal MD value)
  # 
  if (number_of_studies > 1) {
    # setup the page parameters to fit all graphs onto one page
    # based on the number of studies present
    if (plot == TRUE)  {
      lenNV_1  <- number_of_studies - 1
      nframe.X <- ceiling(lenNV_1 ^ 0.5)
      nframe.Y <- ceiling(lenNV_1 / nframe.X)
      op       <- par(mfrow = c((nframe.Y), (nframe.X)))
    }
    # end plot
    
    # extract response data without information columns into pda
    pda <-
      apply(as.data.frame(da[, nv]), 2, door_norm) # processing data
    # pda contains the columns (receptors) of the data matrix
    # the function door_norm has normalized each to [0,1]
    
    ###
    # --> start the merging process: find the first pair
    
    candidate.studies <- colnames(pda)
    # candidate.studies contains a list of all studies in this receptor
    selected <-
      select_model(
        candidate = candidate.studies,
        merged_data = NULL,
        overlapValues,
        data_candidate = pda,
        merged = FALSE
      )
    # selected contains the pair of studies with the best match
    # including all parameters for the best match
    
    if (names(selected$best.model) == "no.fitted.model") {
      message("Can not find any fitted model to merge given data.")
      excluded <- candidate.studies
      merged_data <- rep(NA, nrow(da))
      resd <-
        cbind(da[, c("Class", "Name", "InChIKey", "CID", "CAS")], merged_data)
      return(
        list(
          source.data = character(),
          model.response = resd,
          door_excluded_data = excluded
        )
      )
    } else if (selected[[1]][[1]]$MD >= select.MDValue) {
      message("Can not find any fitted model with MD value under given threshold
               criterion.")
      excluded <- candidate.studies
      merged_data <- rep(NA, nrow(da))
      resd <-
        cbind(da[, c("Class", "Name", "InChIKey", "CID", "CAS")], merged_data)
      return(
        list(
          source.data = character(),
          model.response = resd,
          door_excluded_data = excluded
        )
      )
    }
    
    
    # merge these two datasets to one model response. done by projecting
    # response values onto the chosen function (and, if plot==TRUE, also plot
    # this) use the function (best.model) found with select_model above
    
    projected <-
      project_points(
        x = pda[, selected$selected.x],
        y = pda[, selected$selected.y],
        best.model = selected$best.model,
        xlab = selected$selected.x,
        ylab = selected$selected.y,
        plot = plot,
        title = plot
      )
    
    # the output of project_points is a list with odor responses, either
    # observed in both studies, or only in one study. to integrate it into pda,
    # change data type: list to vector
    merged_data                                   <-
      rep(NA, length = dim(pda)[1])
    merged_data[projected$double.observations$ID] <-
      projected$double.observations$NDR
    merged_data[projected$single.observations$ID]  <-
      projected$single.observations$NDR
    
    #remove the merged studies from the data matrix pda
    rest_data <-
      colnames(pda)[-match(c(selected$selected.y, selected$selected.x),
                           colnames(pda))]
    
    # merge the remaining studies always merge into the model response while
    # there are still studies to process: find the study to be merged next 
    # (again, use calculate_model() to try all studies and fitting functions -->
    # MD values)
    
    ind <- length(rest_data) # how many studies are left to merge
    
    while (ind > 0) {
      # find the best match
      selected.next <-
        select_model(
          candidate = rest_data,
          data_candidate = pda,
          overlapValues,
          merged_data = merged_data,
          merged = TRUE
        )
      # if a threshold for the MD is given, stop when no study reaches this
      # threshold criterion any more
      if ((names(selected.next$best.model) == "initial") |
          (names(selected.next$best.model) == "no.fitted.model") |
          (selected.next$best.model[[1]]$MD > select.MDValue))	{
        excluded  <- c(excluded, rest_data)
        message("Not all datasets could be merged because they did not reach the
                 criterion.")
        ind <- 0
        next
      }
      
      # best match has been found, now merge it into the model response
      # "merged_data" done by projecting response values onto the chosen
      # function (and, if plot==TRUE, also plot this) use the function
      # (best.model) found with select_model above
      
      projected <-
        project_points(
          x = pda[, selected.next$selected.x],
          y = merged_data,
          best.model = selected.next$best.model,
          xlab = selected.next$selected.x,
          ylab = selected.next$selected.y,
          plot = plot,
          title = plot
        )
      
      # overwrite merged_data with new values
      merged_data                                     <-
        rep(NA, length = dim(pda)[1])
      merged_data[projected$double.observations$ID]   <-
        projected$double.observations$NDR
      merged_data[projected$single.observations$ID]    <-
        projected$single.observations$NDR
      #remove the merged study from the data matrix pda
      rest_data <-
        rest_data[-match(selected.next$selected.x, rest_data)]
      ind <- ind - 1
    } # while (ind > 0)
    
    # now all studies have been merged into model response (merged_data)
  } # end if (number_of_studies > 1)
  
  
  
  # global normalization
  if (glob.normalization == TRUE) {
    name.Stud    <- colnames(da)[nv]
    mp_orx       <- match(colnames(da)[nv], responseRange[, "study"])
    Rmax         <-
      apply(as.matrix(da[, nv]), 2, function(x)
        max(range(x, na.rm = TRUE)))
    Smax         <- responseRange[mp_orx, "max"]
    merged_data  <-
      global_norm(
        RMAX = Rmax,
        SMAX = Smax,
        MV = merged_data,
        name.Stud = name.Stud,
        weightGlobNorm = weightGlobNorm,
        responseRange = responseRange
      )
  }
  
  resd <-
    cbind(da[, c("Class", "Name", "InChIKey", "CID", "CAS")], merged_data)
  source.data <- colnames(da)[nv][-na.omit(match(excluded, colnames(da)[nv]))]
  
  return(
    list(
      source.data = source.data,
      model.response = resd,
      door_excluded_data = excluded
    )
  )
}
