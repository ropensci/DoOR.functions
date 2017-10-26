#' Find receptor responses
#'
#' given a chemical, get original receptor responses from all studies in the
#' database.
#'
#' output is a data frame containing response values of given odor across
#' receptors from all available studies.
#'
#' @param odorant a single odor provided as InChIKey
#' @param responseRange data frame, response ranges of studies
#' @param Or.list a list contains reponse data of all available receptors. It
#' can be loaded using \code{\link{load2list}}.
#' @author Daniel Münch \email{daniel.muench@@uni-konstanz.de}
#' @aliases getResponses get_responses
#' @export
#' @keywords data
#' @examples
#' # load data
#' library(DoOR.data)
#' load_door_data(nointeraction = TRUE)
#'
#' # get raw responses for odorant MLFHJEHSLIIPHL-UHFFFAOYSA-N
#' responses <- get_responses(odor = 'MLFHJEHSLIIPHL-UHFFFAOYSA-N')
#'
get_responses <- function(odorant,
                          responseRange = door_default_values("
                                           door_response_range"),
                          Or.list = load2list()) {
  studies  <- responseRange[, "study"]
  Or.Names <- names(Or.list)
  res 	   <- numeric()
  
  for (i in seq_along(Or.Names)) {
    match_odor <- match(odorant, Or.list[[i]][, 'InChIKey'])
    pres1 	   <- Or.list[[i]][match_odor,]
    valueCol   <- as.numeric(which(sapply(pres1, is.numeric)))
    
    if (is.na(valueCol[1])) {
      pres <-
        data.frame(
          ORs = Or.Names[i],
          studies = NA,
          Odor = odorant,
          Response = NA
        )
    } else {
      pres <-
        data.frame(
          ORs = Or.Names[i],
          studies = colnames(pres1)[valueCol],
          Odor = odorant,
          Response = c(as.matrix(pres1[, valueCol]))
        )
    }
    res <- rbind(res, pres)
  }
  return(res)
}
