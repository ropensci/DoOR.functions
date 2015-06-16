#' countStudies
#' 
#' returns a matrix indiating how many studies have recorded individual receptor-odorant combinations
#'
#' @param ors data.frame containing all receptors exidting in DoOR.
#' @param odor.data data.frame containing information about the odorants in DoOR.
#' @param char.columns number of character columns in each receptor data.frame.
#' @param ident odorant identifier to be used as rownames.
#'
#' @return matrix
#'
#' @examples 
#' count <- countStudies()
#' image(count)
#' 
countStudies <-
  function(ors = default.val("ORs"), odor.data = default.val("odor.data"), char.columns = default.val("num.charColumns"), ident = default.val("ident")) {
    counts <- matrix(nrow = dim(odor)[1], dimnames = c(odor[ident]))
    for (i in 1:length(ors$OR)) {
      or.name <- as.character(ors$OR[i])
      or <- get(or.name)
      or <- or[-c(1:char.columns)]
      or <- !is.na(or)
      or <- apply(or, 1, sum)
      counts <- cbind(counts,  or)
      colnames(counts)[i] <- or.name
    }
    return(counts)
  }
