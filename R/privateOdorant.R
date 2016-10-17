#' privateOdorant
#'
#' return an odorant that activates the receptor of interest exclusively
#'
#' @param receptor character, name of a DoOR responding unit (one of \code{ORs$Or})
#' @param sensillum logical, restrict the search to the sensillum the receptor is expressed in?
#' @param response_matrix DoOR response matrix, the input data to perform the search on
#' @param DoOR_mappings the data frame containing the mapping information
#' @param zero character, an odorant that should be set to 0
#' @param nshow numeric, the number of private odorants to return
#' @param tag character, the chemical identifier to give the odorant names in (on of \code{colnames(odor)})
#'
#' @return a data.frame containing odorants and the response in the receptor of interest as well as the maximum response of the remaining receptors and their difference
#' @export
#' @importFrom stats na.omit
#'
#' @examples
#' library(DoOR.data)
#' privateOdorant("Gr21a.Gr63a", tag = "Name")
#' privateOdorant("Or22a", tag = "Name", sensillum = TRUE)
#'
privateOdorant <- function(receptor,
                           sensillum = FALSE,
                           response_matrix = door_default_values("response.matrix"),
                           DoOR_mappings = door_default_values("DoOR_mappings"),
                           zero = door_default_values("zero"),
                           nshow = 5,
                           tag) {
  if(!zero == "")
    response_matrix <- resetSFR(response_matrix, zero)

  if(sensillum == TRUE) {
    sensillum <- as.character(DoOR_mappings$sensillum[which(DoOR_mappings$receptor == receptor)])
    receptors <- as.character(DoOR_mappings$receptor[which(DoOR_mappings$sensillum == sensillum)])
    receptors <- colnames(response_matrix)[which(colnames(response_matrix) %in% receptors)]
    response_matrix <- response_matrix[ ,receptors]
    message(paste0("\n>> Checking only against the ", sensillum, " sensillum (", paste(receptors, collapse =", "),") <<"))
    if(!any(receptors == receptor))
      stop("The receptor you are interested in is not expressed in the sensillum you selected, please check!")
  }

  pos <- which(colnames(response_matrix) == receptor)
  tmp <- data.frame(x          = response_matrix[ ,pos],
                    max.others = suppressWarnings(apply(as.data.frame(response_matrix[ ,-pos]), 1, max, na.rm = TRUE)),
                    n          = apply(as.data.frame(response_matrix[ ,-pos]), 1, function(x) length(na.omit(x))))
  tmp$difference <- tmp$x - tmp$max.others
  tmp <- subset(tmp, !is.na(x) & n > 0)
  colnames(tmp)[1] <- receptor

  if(!missing(tag))
    rownames(tmp) <- transID(rownames(tmp), "InChIKey", tag)

  tmp <- tmp[order(tmp$diff, decreasing = TRUE), ][c(1:nshow), ]

  return(tmp)
}
