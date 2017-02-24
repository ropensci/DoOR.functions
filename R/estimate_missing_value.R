#' Estimate the missing entries in a response data
#'
#' Estimate the missing entries in a response data
#'
#' A wrapper programe for using Pearson Correlation or k-nearest neighbors to
#' estimate the missing entries in a response matrix.
#'
#' @param data a data frame or matrix, contaning the consensus response values
#' @param nodor a numeric value, specifying the number of the selected odors
#' @param method character string, specifying the method ("PC" (Pearson's
#' coefficient) and "Knn" (k nearest neighbors)) for estimation, the default is
#' "PC".
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @aliases DoOREst estimate_missing_value
#' @references Kim, H., Golub, G. H. & Park, H., Missing value estimation for
#' DNA microarray gene expression data: local least squares imputation., 2005,
#' Bioinformatics, 21, 187-198
#' @export
#' @keywords math
#' @examples
#' \dontrun{
#' library(DoOR.data)
#' data(door_response_matrix)
#' subset <- door_response_matrix[1:100, 11:30]
#' est.data <- estimate_missing_value(data = subset, nodor = 6)
#' }
estimate_missing_value <-
  function(data, nodor, method = "PC") {

  # find where is missing in the data
  inputdata <- as.matrix(data)
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
                                     response_matrix = inputdata,
                                     nodor=nodor),TRUE)
    }

    if (method == "PC") {
      tryestmatine <- try(LLSIestPC(InChIKey = inchikey,
                                    receptor = receptor,
                                    response_matrix = inputdata,
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
