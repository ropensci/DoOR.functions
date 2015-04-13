#' Remove a study from DoOR
#' 
#' Use this function to remove a study from the DoOR database. 
#' \code{importNewData.R} uses this function when it detects an existing study 
#' during the import process (e.g. because you imported updated data).
#' @param study a string containing the name of the study you want to remove 
#'   (e.g. 'Bruyne.2001.WT')
#' @param receptors a vector of all the receptors to be checked. Defaults to all
#'   receptors exidting in DoOR.
#' @param responseRange the dataframe containing the info about the response 
#'   ranges of all studies (\code{response.range})
#' @param weightGlobNorm the dataframe containing the info about the relative
#'   weights between receptors (\code{weight.globNorm})
#'   
#' @seealso \code{\link{importNewData}}
#'   
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#'   
#' @examples
#' removeStudy('Bruyne.2001.WT')
#' 
removeStudy <- function(study,
                        receptors = default.val('ORs'),
                        responseRange = default.val('response.range'),
                        weightGlobNorm = default.val('weight.globNorm')) {
  for(x in receptors[,'OR']) {
    data <- try(get(x), silent = TRUE)
    if (any(colnames(data) == study)) {
      pos <- which(colnames(data) == study)
      data <- data[,-pos]
      assign(x, data, envir = .GlobalEnv)
      message(paste('removed',study,'from',x,'.'))       
    }
  }
  if(study %in% responseRange$study) {
    responseRange <- responseRange[- which(responseRange$study == study),] 
    assign('response.range', responseRange, envir = .GlobalEnv)
    message(paste('removed',study,'from \'response.range\''))
  } else {
    warning(paste(study, 'not found in \'response.range\''))
  }
 
  if(study %in% names(weightGlobNorm)) {
    weightGlobNorm <- weightGlobNorm[, - which(colnames(weightGlobNorm) == study)]
    assign('weight.globNorm', weightGlobNorm, envir = .GlobalEnv)
    message(paste('removed',study,'from \'weight.globNorm\''))
  } else {
    warning(paste(study, 'not found in \'weight.globNorm\''))
  }
}