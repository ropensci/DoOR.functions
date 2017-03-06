#' update_door_odorinfo
#'
#' Update the DoOR odor data with info from \code{odor}. For the function to
#' work, all DoOR data has to be loaded to the current environment.
#'
#' @author Daniel Münch, \email{daniel@@muench.bio}
#' @aliases updateOdorInfo update_door_odorinfo
#' @export
#' @examples 
#' # modify odor
#' odor[1,1] <- "acid"
#' 
#' # run 
#' update_door_odorinfo()
#' 
#' # check that data sets have been updated
#' head(Or22a)
#'
#'
update_door_odorinfo <- function() {
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
  assign('door_data_format', odor[1:5], envir = .GlobalEnv)
}
