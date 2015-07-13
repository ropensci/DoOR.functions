#' getKey
#' 
#' Translate an odor identifier (e.g. CAS, Name, ...) to InChIKey, the DoOR default identifier.
#'
#' @param x character, an odor identifier
#' @param type the type od identifier (one of the column names of ``odor``)
#' @param odor.data the data frame containing the odor information (defaults to ``odor``).
#'
#' @return character, an InChIKey
#' @export
#'
#' @examples 
#' getKey("123-92-2")
#' getKey("isopentyl acetate", "Name")
#' getKey("C(C(C)C)COC(=O)C", "SMILES")
#' 
#' DoORplot_ALmap(getKey("123-92-2"))
getKey <- function(x, type = "CAS", odor.data = default.val("odor.data")) {
  result <- odor.data$InChIKey[which(odor.data[,type] == x)]
  if(is.na(x)) {
    warning("Entered NA, returning NA.")
    return(NA)
  }
  if(length(result) > 1) {
    warning("More than one result, returning first hit.")
    return(as.character(result[1]))
  }
  if(length(result) == 0) {
    warning("No match, returning NA.")
    return(NA)
  }
  return(as.character(result))
}
