#' Compose a Response Matrix of All Odor Receptors
#' 
#' computes the complete response model for all receptors in the database
#' (calls \code{\link{modelRP}} for all receptors)
#' 
#' 
#' @param tag character string, format for rownames, possibilities: "InChIKey",
#'   CAS", "CID", "Name"
#' @param select.MDValue a numeric, threshold on the MD, this is used to reject 
#'   studies that do not align sufficiently well to the response model
#' @param overlapValues numeric, a criterion using to refuse a data set that
#' has not enough overlap value.
#' @param ... pass more parameters to \code{\link{modelRP}}
#' @seealso \code{\link{modelRP}}
#' @keywords data
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Shouwen Ma <\email{daniel.muench@@uni-konstanz.de}>
#' @export
#' @examples
#' \dontrun{
#' library(DoOR.data)
#' loadData()
#' mydatabase <- CreateDatabase()
#' }
CreateDatabase <- function(tag                = default.val("tag"), 
                           select.MDValue     = default.val("select.MDValue"), 
                           overlapValues      = default.val("overlapValues"), 
                           ...) {
  
  excluded.data   <- data.frame(OR = ORs$OR, excluded = NA) # reset/create excluded.data
  
  Or.list  	      <- load2list() 	# contains data for all receptors
  Or.Names 	      <- names(Or.list)
  num_receptors 	<- length(Or.Names)	# how many receptors
  
  odors <- character()
  for (i in Or.Names) {
    pre.odors <- as.vector(Or.list[[i]][,tag])
    new_odors <- which(is.na(match(pre.odors,odors)))
    odors     <- c(odors,pre.odors[new_odors])
  }
  
  num_odors            <- length(odors)					# how many odors
  frame_data           <- matrix(NA, nrow = num_odors, ncol = num_receptors)	# empty matrix
  colnames(frame_data) <- Or.Names
  rownames(frame_data) <- odors
  
  for (i in Or.Names) {
    da <- Or.list[[i]]
    
    # if no response data, fill in "NA" and skip
    if (dim(da)[2] <= default.val("num.charColumns")) { 
      print(paste(i, "is a empty data frame."))
      frame_data[, i] <- NA 
    } else {
      merged <- modelRP(da, select.MDValue, overlapValues, glob.normalization = TRUE, ...)
      merged.responses   <- merged$model.response[,"merged_data"]
      excluded           <- merged$excluded.data
      merged.odors       <- as.vector(merged$model.response[,tag])
      match_odorsTOframe <- match(merged.odors, rownames(frame_data))
      frame_data[match_odorsTOframe, i] <- merged.responses
      # update response.matrix.excluded
      if (length(excluded > 0))
        excluded.data[excluded.data$OR == i, "excluded"] <- paste(excluded, collapse = ", ")
        
      print(paste(i, "has been merged."))
    }
  }
  frame_data_nn <- apply(frame_data, 2, DoORnorm)
  
  assign("response.matrix", frame_data, envir = .GlobalEnv)
  message("response.matrix has been created")
  
  assign("response.matrix_non.normalized", frame_data_nn, envir = .GlobalEnv)
  message("response.matrix_non.normalized has been created")
  
  assign("excluded.data", excluded.data, envir = .GlobalEnv)
  message("excluded.data has been created")
}
