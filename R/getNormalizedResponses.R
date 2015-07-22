#' Find normalised receptor responses
#' 
#' given a chemical, get normalised receptor responses from all studies in the 
#' database.
#' 
#' 
#' @param odors character vector; one or more odors provided as InChIKey.
#' @param zero InChIKey of background that should be set to zero. The default is
#'   "SFR", i.e. the spontaneous firing rate.
#' @param response_matrix a data frame; as e.g. "response.matrix" that is loaded
#'   by \code{\link{modelRP}}. It is also possible to create this frame manually
#'   using \code{\link{modelRP}}.
#' @seealso \code{\link{modelRP}},\code{\link{CreateDatabase}}
#' @export
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' odors <- c("MLFHJEHSLIIPHL-UHFFFAOYSA-N",
#'            "OBNCKNCVKJNDBV-UHFFFAOYSA-N",
#'            "IKHGUXGNUITLKF-UHFFFAOYSA-N")
#' data(response.matrix)
#' result <- getNormalizedResponses(odors, response_matrix = response.matrix)
#' 
getNormalizedResponses <- function(odors, zero = default.val("zero"), response_matrix = default.val("response.matrix")) {
  
  response_matrix <- apply(response_matrix, 2, function(x) resetSFR(x,x[zero]))
  
  mp  <- match(odors,rownames(response_matrix))
  if(any(is.na(mp))) {
    stop(paste("The following odorants are not in the database: "), paste(odors[which(is.na(mp))], collapse = ", "))
  }
    
  res <- data.frame(ORs = rep(colnames(response_matrix),each=length(odors)),
                    Odor = rep(odors,dim(response_matrix)[2]),
                    Response = c(as.matrix(response_matrix[mp,])))
  return(res)
}
