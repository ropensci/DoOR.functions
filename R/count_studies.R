#' count_studies
#'
#' returns a matrix indiating how many studies have recorded individual
#' receptor-odorant combinations
#'
#' @param ors data.frame containing all receptors exidting in DoOR.
#' @param odor_data data.frame containing information about the odorants in
#'   DoOR.
#' @param char.columns number of character columns in each receptor data.frame.
#' @param ident odorant identifier to be used as rownames.
#' @export
#' @aliases countStudies, count_studies
#'
#' @return matrix
#'
#' @examples
#' library(DoOR.data)
#' load_door_data()
#' count <- countStudies()
#' image(count)
#'
count_studies <- function(ors = default.val("ORs"),
                         odor_data = default.val("odor"),
                         char.columns = default.val("num.charColumns"),
                         ident = default.val("ident")) {
  counts <- matrix(nrow = dim(odor_data)[1], dimnames = c(odor_data[ident]))
  for (i in 1:length(ors$OR)) {
    or.name <- as.character(ors$OR[i])
    or <- get(or.name)
    or <- or[-c(1:char.columns)]
    or <- !is.na(or)
    or <- apply(or, 1, sum)
    if(i == 1) {
      counts[,1] <- or
    } else {
      counts <- cbind(counts,  or)
    }
    colnames(counts)[i] <- or.name
  }
  return(counts)
}
