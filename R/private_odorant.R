#' private_odorant
#'
#' return an odorant that activates the receptor of interest exclusively
#'
#' @param receptor character, name of a DoOR responding unit (one of
#'   \code{ORs$Or})
#' @param sensillum logical, restrict the search to the sensillum the receptor
#'   is expressed in?
#' @param response_matrix DoOR response matrix, the input data to perform the
#'   search on
#' @param door_mappings the data frame containing the mapping information
#' @param zero character, an odorant that should be set to 0
#' @param nshow numeric, the number of private odorants to return
#' @param tag character, the chemical identifier to give the odorant names in
#'   (on of \code{colnames(odor)})
#'
#' @return a data.frame containing odorants and the response in the receptor of
#'   interest as well as the maximum response of the remaining receptors and
#'   their difference
#' @export
#' @importFrom stats na.omit
#' @aliases privateOdorant private_odorant
#'
#' @examples
#' # load data
#' library(DoOR.data)
#'
#' # find a private odorant for Gr21a.Gr63a (the carbon dioxide receptor)
#' # private_odorant("Gr21a.Gr63a", tag = "Name")
#'
#' # now find an odorant that within the ab3 sensillum specifically activates
#' # Or22a
#' private_odorant("Or22a", tag = "Name", sensillum = TRUE)
#'
private_odorant <- function(receptor,
                  sensillum = FALSE,
                  response_matrix = door_default_values("door_response_matrix"),
                  door_mappings = door_default_values("door_mappings"),
                  zero = door_default_values("zero"),
                  nshow = 5,
                  tag) {
  if (!zero == "")
    response_matrix <- reset_sfr(response_matrix, zero)
  
  if (sensillum == TRUE) {
    sensillum <-
      as.character(door_mappings$sensillum[
        which(door_mappings$receptor == receptor)])
    receptors <-
      as.character(door_mappings$receptor[
        which(door_mappings$sensillum == sensillum)])
    receptors <-
      colnames(response_matrix)[which(colnames(response_matrix) %in% receptors)]
    response_matrix <- response_matrix[, receptors]
    message(paste0(
      "\n>> Checking only against the ",
      sensillum,
      " sensillum (",
      paste(receptors, collapse = ", "),
      ") <<"
    ))
    if (!any(receptors == receptor))
      stop(
        "The receptor you are interested in is not expressed in the sensillum 
         you selected, please check!"
      )
  }
  
  pos <- which(colnames(response_matrix) == receptor)
  tmp <- data.frame(
    x          = response_matrix[, pos],
    max.others = suppressWarnings(apply(
      as.data.frame(response_matrix[, -pos]), 1, max, na.rm = TRUE
    )),
    n          = apply(as.data.frame(response_matrix[, -pos]), 1, function(x)
      length(na.omit(x)))
  )
  tmp$difference <- tmp$x - tmp$max.others
  tmp <- subset(tmp,!is.na(x) & n > 0)
  colnames(tmp)[1] <- receptor
  
  if (!missing(tag))
    rownames(tmp) <- trans_id(rownames(tmp), "InChIKey", tag)
  
  tmp <- tmp[order(tmp$diff, decreasing = TRUE),][c(1:nshow),]
  
  return(tmp)
}
