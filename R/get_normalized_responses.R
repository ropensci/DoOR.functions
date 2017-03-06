#' Find normalised receptor responses
#'
#' given a chemical, get normalised receptor responses from all studies in the
#' database.
#'
#'
#' @param odors character vector, one or more odors provided as InChIKey.
#' @param zero InChIKey of background that should be set to zero. The default is
#'   "SFR", i.e. the spontaneous firing rate.
#' @param response_matrix a data frame, as e.g. "door_response_matrix" that is loaded
#'   by \code{\link{model_response}}. It is also possible to create this frame manually
#'   using \code{\link{model_response}}.
#' @param round numeric, round to this amount of digits, set to NA if you do not want to round
#' @param na.rm logical, remove NAs?
#' @seealso \code{\link{model_response}},\code{\link{create_door_database}}
#' @export
#' @importFrom stats na.omit
#' @keywords data
#' @author Daniel Münch \email{daniel.muench@@uni-konstanz.de}
#' @aliases getNormalizedResponses get_normalized_responses
#' @examples
#' # load data
#' library(DoOR.data)
#' data(door_response_matrix)
#' 
#' # define a list of odorants
#' odors <- c("MLFHJEHSLIIPHL-UHFFFAOYSA-N",
#'            "OBNCKNCVKJNDBV-UHFFFAOYSA-N",
#'            "IKHGUXGNUITLKF-UHFFFAOYSA-N")
#' 
#' # get the normalized responses for these odorants
#' result <- get_normalized_responses(odors, response_matrix = door_response_matrix)
#'
get_normalized_responses <- function(odors,
                                   zero = door_default_values("zero"),
                                   response_matrix = door_default_values("door_response_matrix"),
                                   round = 3,
                                   na.rm = FALSE) {

  response_matrix <- reset_sfr(response_matrix, zero)

  mp  <- match(odors,rownames(response_matrix))
  if(any(is.na(mp))) {
    stop(paste("The following odorants are not in the database: "), paste(odors[which(is.na(mp))], collapse = ", "))
  }

  Response <- c(as.matrix(response_matrix[mp,]))
  if(!is.na(round))
    Response <- round(Response, round)

  res <- data.frame(ORs      = rep(colnames(response_matrix),each=length(odors)),
                    Odor     = rep(odors,dim(response_matrix)[2]),
                    Response = Response)

  if(na.rm == TRUE)
    res <- na.omit(res)

  return(res)
}
