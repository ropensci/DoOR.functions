#' load data into a list
#' 
#' load all available data into a list
#' 
#' Please load ORs from data package DoOR.data by typing (\code{data(ORs)}) 
#' before use.
#' 
#' @param odorantReceptors data frame; containing receptor or ORN names and 
#'   their expression (2: both in adult and larvae; 1: only in larvae, 0: only
#'   in adult). Output is a list containing all measurements from all available
#'   studies.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @export
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' library(DoOR.function)
#' loadRD()
#' orlist <- loadRDList()
#' orlist$Or22a
#' 
loadRDList <- function(odorantReceptors = default.val("ORs")) {
  
	Or.list  <- vector(length = 0,"list")
	or.names <- as.vector(odorantReceptors[,"OR"])
	n 	 <- length(or.names)

	for (i in 1:n) {
		OR.name <- or.names[i]
		Or.list[[OR.name]] <- get(OR.name)
	}

return(Or.list)
}
