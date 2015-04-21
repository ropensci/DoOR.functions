#' Find normalised receptor responses
#' 
#' given a chemical, get normalised receptor responses from all studies in the
#' database.
#' 
#' 
#' @param odors character vector; one or more odors provided as InChIKey.
#' @param zero InChIKey of background that should be set to zero. The default is "SFR", i.e. the spontaneous
#' firing rate.
#' @param responseMatrix a data frame; as e.g. "response.matrix" that is loaded
#' by \code{\link{modelRP}}. It is also possible to create this frame manually
#' using \code{\link{modelRP}}.
#' @seealso \code{\link{modelRP}},\code{\link{CreateDatabase}}
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' odors <- c("MLFHJEHSLIIPHL-UHFFFAOYSA-N","OBNCKNCVKJNDBV-UHFFFAOYSA-N","IKHGUXGNUITLKF-UHFFFAOYSA-N")
#' data(response.matrix)
#' result <- findRespNorm(cas, responseMatrix = response.matrix)
#' 
findRespNorm <- function(odors, zero = default.val("zero"), responseMatrix = default.val("response.matrix")) {

	Or.Names <- colnames(responseMatrix)
	n	 <- dim(responseMatrix)[2]
	
	for (j in 1:n) {
		if (is.na(which(!is.na(responseMatrix[,j]))[1])) { 
      next 
		} else {
			# reset the response data by setting "zero" to 0, the default "zero" is spontanous firing rate (SFR)
			if (is.na(responseMatrix[zero,j])) { 
        mzero <- 0 
			} else { 
        mzero <- responseMatrix[zero,j] 
			}
			responseMatrix[,j] <- resetSFR(responseMatrix[,j], mzero)	
		}
	}
	
	mp  <- match(odors,rownames(responseMatrix))
	res <- data.frame(ORs = rep(colnames(responseMatrix),each=length(odors)),
			              Odor = rep(odors,length(Or.Names)),
			              Response = c(as.matrix(responseMatrix[mp,])))
return(res)
}
