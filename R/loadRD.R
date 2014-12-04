loadRD <-
function(odorantReceptors = default.val("ORs"), supportData = TRUE) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# LoadRD.R:
###########

# Load all datasets including the precomputed response model.

# input parameters:
###################

# odorantReceptors : data frame; containing receptor or ORN names and their expression status (2: both in adult and larvae; 1: only in larvae, 0: only in adult)
# supportData      : logical; if TRUE, load supporting information, too

# output: data is loaded into the R workspace

{
	receptor.name 	 <- as.character(odorantReceptors[,"OR"])
	supportData.name <- c("AL256", "data.format", "glo.dist", "odor", "OGN", "ORs", "reference","response.matrix", "response.range","unglobalNorm_response.matrix","weight.globNorm","dataset.info")
	data.name 	 <- c(receptor.name,supportData.name)

	# version information of data and function
	version.data()
	version.function()

	return(list(data(list=(data.name))))


}
