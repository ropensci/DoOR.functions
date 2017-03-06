#' getDataset
#'
#' aggregates original data from a given study
#'
#' @param study character, the name of the study you want to aggregate the dta
#'   from
#' @param na.rm logical, whether or not to exclude odorants that were not
#'   measured in the study
#'
#' @return returns a data frame containing all the odorant responses measured in
#'   \code{study}
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases get_dataset getDataset
#' @export
#'
#' @examples
#' # load data
#' library(DoOR.data)
#' load_door_data()
#' 
#' # get all recordings from the Hallem.2004.EN data set
#' get_dataset("Hallem.2004.EN", na.rm = TRUE)
get_dataset <- function(study, na.rm = FALSE) {
  tmp <- load2list()
  selected <- sapply(tmp, "[[", study)
  selected.length <- sapply(selected, function(x) length(x))
  selected.pos <- which(selected.length > 0)

  selected <- as.data.frame(selected[selected.pos])
  result <- cbind(door_data_format, selected)

  if(na.rm == T) {
    nonNA <- which(apply(!is.na(selected), 1, sum) > 0)
    result <- result[nonNA,]
  }

  return(result)
}
