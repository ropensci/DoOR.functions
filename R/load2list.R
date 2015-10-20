#' load2list
#'
#' returns all original DoOR response data as a list
#'
#' @return a list
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @export
#'
#' @examples
#' library(DoOR.data)
#' loadData()
#' lst <- load2list()
load2list <- function() {
  if(!exists("ORs"))
    stop("Load data first (loadData())")
  
  lst <- list()
  for(x in ORs$OR) {
    lst[[x]] <- get(x)
  }
  return(lst)
}
