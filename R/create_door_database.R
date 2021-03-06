#' Compose a Response Matrix of All Odor Receptors
#'
#' computes the complete response model for all receptors in the database (calls
#' \code{\link{model_response}} for all receptors). Overwrites response_matrix,
#' door_response_matrix_non_normalized and door_excluded_data.
#'
#'
#' @param tag character string, format for rownames, possibilities: "InChIKey",
#'   CAS", "CID", "Name"
#' @param select.MDValue a numeric, threshold on the MD, this is used to reject
#'   studies that do not align sufficiently well to the response model
#' @param overlapValues numeric, a criterion using to refuse a data set that
#' has not enough overlap value.
#' @param ... pass more parameters to \code{\link{model_response}}
#' @seealso \code{\link{model_response}}
#' @keywords data
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases CreateDatabase create_door_database
#' @export
#' @examples
#' \dontrun{
#' # load DoOR data
#' library(DoOR.data)
#' load_door_data()
#' 
#' # create a new consensus matrix
#' create_door_database()
#' }
create_door_database <-
  function(tag                = door_default_values("tag"),
           select.MDValue     = door_default_values("select.MDValue"),
           overlapValues      = door_default_values("overlapValues"),
           ...) {
    door_excluded_data   <-
      data.frame(OR = get('ORs')$OR, excluded = NA) #recreate door_excluded_data
    
    Or.list       <- load2list() 	# contains data for all receptors
    Or.Names      <- names(Or.list)
    num_receptors <- length(Or.Names)	# how many receptors
    
    odors <- character()
    for (i in Or.Names) {
      pre.odors <- as.vector(Or.list[[i]][, tag])
      new_odors <- which(is.na(match(pre.odors, odors)))
      odors     <- c(odors, pre.odors[new_odors])
    }
    
    num_odors            <- length(odors)					# how many odors
    frame_data           <-
      matrix(NA, nrow = num_odors, ncol = num_receptors)	# empty matrix
    colnames(frame_data) <- Or.Names
    rownames(frame_data) <- odors
    
    for (i in Or.Names) {
      da <- Or.list[[i]]
      
      # if no response data, fill in "NA" and skip
      if (dim(da)[2] <= door_default_values("num.charColumns")) {
        print(paste(i, "is a empty data frame."))
        frame_data[, i] <- NA
      } else {
        merged <-
          model_response(da,
                         select.MDValue,
                         overlapValues,
                         glob.normalization = TRUE,
                         ...)
        merged.responses   <- merged$model.response[, "merged_data"]
        excluded           <- merged$door_excluded_data
        merged.odors       <- as.vector(merged$model.response[, tag])
        match_odorsTOframe <-
          match(merged.odors, rownames(frame_data))
        frame_data[match_odorsTOframe, i] <- merged.responses
        # update door_response_matrix.excluded
        if (length(excluded > 0))
          door_excluded_data[door_excluded_data$OR == i, "excluded"] <-
          paste(excluded, collapse = ", ")
        
        print(paste(i, "has been merged."))
      }
    }
    frame_data_nn <- apply(frame_data, 2, door_norm)
    
    assign("door_response_matrix", frame_data, envir = .GlobalEnv)
    message("door_response_matrix has been created")
    
    assign("door_response_matrix_non_normalized",
           frame_data_nn,
           envir = .GlobalEnv)
    message("door_response_matrix_non_normalized has been created")
    
    assign("door_excluded_data", door_excluded_data, envir = .GlobalEnv)
    message("door_excluded_data has been created")
  }
