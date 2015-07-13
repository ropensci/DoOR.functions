#' Estimate the missing entries in a response data
#' 
#' Estimate the missing entries in a response data
#' 
#' A warp programe using \code{\link{LLSIestPC}} or \code{\link{LLSIestKnn}}
#' estimate the missing entries in a response data.
#' 
#' @param da a data frame or matrix; contaning the consensus response values.
#' @param nodor a numeric value; specifying the number of the selected odors.
#' @param method character string; specifying the method ("PC" (Pearson's
#' coefficient) and "Knn" (k nearest neighbors)) for estimation, the default is
#' "PC".
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @references Kim, H.; Golub, G. H. & Park, H., Missing value estimation for
#' DNA microarray gene expression data: local least squares imputation., 2005,
#' Bioinformatics, 21, 187-198
#' @export
#' @keywords math
#' @examples
#' 
#' library(DoOR.data)
#' data(response.matrix)
#' # est_data <- DoOREst(da=response.matrix, nodor = 3)
#' 
DoOREst <-
function(da, nodor, method = "PC")

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# DoOREst.R :
####################

# estimate NA entries in a consensus response data

# input parameters:
###################

# da 		: data frame or a matrix; a consensus response data.
# nodor 	: numeric; number of selected odors.
# method 	: character string; specifying the method for estimation, default is "Knn" (k nearest neighbors). The alternative is "PC" (Pearson's coefficient).

## output is a consensus response data that also constains the estimated values.

{

	# find where is missing in the data
	inputdata <- as.matrix(da)
	outputdata <- inputdata
	whichmissing <- which(is.na(inputdata),arr.ind=TRUE)
	dim_whichmissing <- dim(whichmissing)
	precentageMissing <- round(dim_whichmissing[1]/length(inputdata),4)*100
	message(paste("There are",dim_whichmissing[1],paste( "(",precentageMissing,"%)",sep=""), "missing values in the given data."))

	for (i in 1:dim_whichmissing[1]) {

	receptor <- colnames(inputdata)[whichmissing[i,2]]
	inchikey 	 <- rownames(inputdata)[whichmissing[i,1]]
	
	if (method == "Knn") {
		tryestmatine <- try(LLSIestKnn(InChIKey = inchikey, 
					receptor = receptor, 
					responseMatrix = inputdata, 
					nodor=nodor),TRUE)
	}

	if (method == "PC") {
		tryestmatine <- try(LLSIestPC(InChIKey = inchikey, 
					receptor = receptor, 
					responseMatrix = inputdata, 
					nodor=nodor),TRUE)
	}

	if (inherits(tryestmatine, "try-error")) { print(paste("The odorant response of",receptor, "to", inchikey, "can not be estimated.")) }
	else { outputdata[inchikey, receptor] <- tryestmatine$estimation }
	if (inherits(tryestmatine, "Warning messages:")) { stop }
	}
	whichmissingOutput 	<- which(is.na(outputdata),arr.ind=TRUE)
	dim_whichmissingOutput 	<- dim(whichmissingOutput)
	precentageMissingOutput <- round(dim_whichmissingOutput[1]/length(outputdata),4)*100
	N_estimate <- dim_whichmissing[1] - dim_whichmissingOutput[1]
	message(paste("There are",dim_whichmissingOutput[1],paste( "(",precentageMissingOutput,"%)",sep=""), "missing values in the output data."))
	message(paste(N_estimate,"values were estimated."))
	return(outputdata)
}
