#' Load Data
#' 
#' Load all datasets including the precomputed response matrix.
#' 
#' @param odorantReceptors data frame; containing receptor or ORN names and
#' their expression.
#' @param supportData logical; if TRUE, load support data.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' library(DoOR.function)
#' loadRD()
#' 
loadRD <- function(odorantReceptors = default.val("ORs"), supportData = TRUE) {
	receptor.name 	 <- as.character(odorantReceptors[,"OR"])
	supportData.name <- c("AL256", "data.format", "glo.dist", "odor", "OGN", "ORs", "reference","response.matrix", "response.range","unglobalNorm_response.matrix","weight.globNorm","dataset.info")
	data.name 	 <- c(receptor.name,supportData.name)

	# version information of data and function
	version.data()
	version.function()

	return(list(data(list=(data.name))))


}
