#' update_odorinfo
#'
#' Update the DoOR odor data with info from \code{odor}. For the function to
#' work, all DoOR data has to be loaded.
#'
#' @author Daniel Münch, \email{daniel@@muench.bio}
#' @aliases updateOdorInfo update_odorinfo
#' @export
#'
#'
update_odorinfo <- function() {
  for(i in ORs$OR)
  {
    tmp <- get(i, envir = .GlobalEnv)
    match <- match(tmp$InChIKey, odor$InChIKey)
    if (length(tmp) > 5)
    {
      tmp <- cbind(odor[1:5], tmp[6:length(tmp)])
    } else {
      tmp <- odor[1:5]
    }
    assign(i, tmp, envir = .GlobalEnv)
  }
  assign('data.format', odor[1:5], envir = .GlobalEnv)
}
