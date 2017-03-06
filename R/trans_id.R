#' trans_id
#'
#' Translate chemical identifiers from one to the other.
#'
#' @param x character vector, one or many chemical identifiers
#' @param from character, the type of identifier to translate from (one of the column names of ``odor``)
#' @param to character, the type of identifier to translate from (one of the column names of ``odor``)
#' @param odor_data the data frame containing the odor information (defaults to ``odor``).
#'
#' @return character vector of translated chemical identifiers
#' @export
#' @aliases transID trans_id
#'
#' @examples
#' # load data
#' library(DoOR.data)
#' 
#' # transform CAS to InChIKey
#' trans_id("123-92-2")
#' 
#' # transform Name to InChIKey
#' trans_id("isopentyl acetate", "Name")
#' 
#' # transform SMILE to InChIKey
#' trans_id("C(C(C)C)COC(=O)C", "SMILES", "Name")
#'
trans_id <- function(x, from = "CAS", to = "InChIKey", odor_data = door_default_values("odor")) {
  result <- odor_data[match(x, odor_data[ , from]), to]
 if(length(result) == 0) {
    warning("No match, returning NA.")
    return(NA)
  }
  return(as.character(result))
}
