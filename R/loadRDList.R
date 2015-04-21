#' load data into a list
#' 
#' load all available data into a list
#' 
#' Please load ORs from data package DoOR.data by typing (\code{data(ORs)})
#' before use.
#' 
#' @param odorantReceptors data frame; containing receptor or ORN names and
#' their expression.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' library(DoOR.function)
#' loadRD()
#' orlist <- loadRDList()
#' orlist$Or22a
#' 
loadRDList <-
function(odorantReceptors = default.val("ORs"))

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# loadRDlist.R:
################

# load all available data into a list 


# input parameters:
###################

# odorantReceptors : data frame; contains receptor or ORN names and their expression. (2: both in adult and larvae; 1: only in larvae, 0: only in adult)

# output is a list containing all measurements from all available studies.

{
	Or.list  <- vector(length = 0,"list")
	or.names <- as.vector(odorantReceptors[,"OR"])
	n 	 <- length(or.names)

	for (i in 1:n) 
	{
		OR.name <- or.names[i]
		Or.list[[OR.name]]<-get(OR.name)
	}

return(Or.list)
}
