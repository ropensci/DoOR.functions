#' privateOdorant
#' 
#' return an odorant that activates the receptor of interes exclusively
#'
#' @param receptor character; name of a DoOR responding unit (one of \code{ORs$Or})
#' @param sensillum logical; restrict the search to the sensillum the receptor is expressed in?
#' @param responseMatrix DoOR response matrix; the input data to perform the search on
#' @param zero character; an odorant that should be set to 0
#' @param nshow numeric; the number of private odorants to return
#' @param tag character; the chemical identifier to give the odorant names in (on of \code{colnames(odor)})
#'
#' @return a data.frame containing odorants and the response in the receptor of interest as well as the maximum response of the remaining receptors and their difference
#' @export
#'
#' @examples 
#' privateOdorant("Gr21a.Gr63a", tag = "Name")
#' privateOdorant("Or22a", tag = "Name", sensillum = T)
#' 
privateOdorant <- function(receptor,
                           sensillum = F,
                           responseMatrix = default.val("response.matrix"),
                           zero = default.val("zero"),
                           nshow = 5, 
                           tag) {
  if(!zero == "") 
    responseMatrix <- resetSFR(responseMatrix, zero)
  
  if(sensillum == T) {
    sensillum <- as.character(DoOR.mappings$sensillum[which(DoOR.mappings$receptor == receptor)])
    receptors <- as.character(DoOR.mappings$receptor[which(DoOR.mappings$sensillum == sensillum)])
    receptors <- colnames(responseMatrix)[which(colnames(responseMatrix) %in% receptors)]
    responseMatrix <- responseMatrix[ ,receptors]
    message(paste0("\n>> Checking only against the ", sensillum, " sensillum (", paste(receptors, collapse =", "),") <<"))
    if(!any(receptors == receptor))
      stop("The receptor you are interested in is not expressed in the sensillum you selected, please check!")
  }
  
  pos <- which(colnames(responseMatrix) == receptor)
  tmp <- data.frame(x          = responseMatrix[ ,pos], 
                    max.others = suppressWarnings(apply(as.data.frame(responseMatrix[ ,-pos]), 1, max, na.rm = T)),
                    n          = apply(as.data.frame(responseMatrix[ ,-pos]), 1, function(x) length(na.omit(x))))
  tmp$difference <- tmp$x - tmp$max.others
  tmp <- subset(tmp, !is.na(x) & n > 0)
  colnames(tmp)[1] <- receptor
  
  if(!missing(tag))
    rownames(tmp) <- transID(rownames(tmp), "InChIKey", tag)
  
  tmp <- tmp[order(tmp$diff, decreasing = T), ][c(1:nshow), ]
  
  return(tmp)
}