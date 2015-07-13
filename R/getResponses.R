#' Find receptor responses
#' 
#' given a chemical, get original receptor responses from all studies in the
#' database.
#' 
#' output is a data frame containing response values of given odor across
#' receptors from all available studies.
#' 
#' @param odorant a single odor provided as InChIKey
#' @param responseRange data frame; response ranges of studies
#' @param Or.list a list contains reponse data of all available receptors. It
#' can be loaded using \code{\link{loadRDList}}.
#' @export
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' loadRD()
#' responses <- getResponses(odor = 'MLFHJEHSLIIPHL-UHFFFAOYSA-N')
#' 
getResponses <- function(odorant, responseRange = default.val("response.range"), Or.list = loadRDList()) {

  studies  <- responseRange[,"study"]
	Or.Names <- names(Or.list)
	res 	   <- numeric()
  
	for (i in 1:length(Or.Names)) {
  	match_odor <- match(odorant, Or.list[[i]][,'InChIKey'])
    pres1 	   <- Or.list[[i]][match_odor, ]
  	valueCol   <- as.numeric(which(sapply(pres1, is.numeric)))
  
  	if (is.na(valueCol[1])) {
  		pres <- data.frame(ORs = Or.Names[i], studies = NA, Odor = odorant, Response = NA )
  	} else {
  		pres <- data.frame(ORs = Or.Names[i], studies = colnames(pres1)[valueCol], Odor = odorant, Response = c(as.matrix(pres1[, valueCol])))
  	}
  	res<-rbind(res,pres)
  }
return(res)
}
