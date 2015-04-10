#' Update the DoOR odor data withinfo from 'odor'
#'
#' @author Daniel Münch, \email{daniel@muench.bio}
#'
#'
updateOdorInfo <- function() 
{
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
  data.format <- odor[1:5]
}