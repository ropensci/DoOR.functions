#' transID
#' 
#' Translate chemical identifiers from one to the other.
#'
#' @param x character vector; one or many chemical identifiers
#' @param from character; the type of identifier to translate from (one of the column names of ``odor``)
#' @param to character; the type of identifier to translate from (one of the column names of ``odor``)
#' @param odor_data the data frame containing the odor information (defaults to ``odor``).
#'
#' @return character vector of translated chemical identifiers
#' @export
#'
#' @examples 
#' transID("123-92-2")
#' transID("isopentyl acetate", "Name")
#' transID("C(C(C)C)COC(=O)C", "SMILES", "Name")
#' 
#' DoORplot_ALmap(getKey("123-92-2"))
transID <- function(x, from = "CAS", to = "InChIKey", odor_data = default.val("odor_data")) {
  result <- odor_data[match(x, odor_data[ , from]), to]
 if(length(result) == 0) {
    warning("No match, returning NA.")
    return(NA)
  }
  return(as.character(result))
}
