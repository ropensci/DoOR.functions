#' load2list
#'
#' returns all original DoOR response data as a list
#'
#' @return a list
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @export
#'
#' @examples
#' # load DoOR.data
#' library(DoOR.data)
#' load_door_data(nointeraction = TRUE)
#'
#' # write the data into a list
#' lst <- load2list()
load2list <- function() {
  if(!exists("ORs"))
    stop("Load data first (load_door_data())")

  lst <- list()
  for(x in ORs$OR) {
    lst[[x]] <- get(x)
  }
  return(lst)
}
