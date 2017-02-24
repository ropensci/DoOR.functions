#' Remove a study from DoOR
#'
#' Use this function to remove a study from the DoOR database.
#' \code{import_new_data.R} uses this function when it detects an existing study
#' during the import process (e.g. because you imported updated data).
#' @param study a string containing the name of the study you want to remove
#'   (e.g. 'Bruyne.2001.WT')
#' @param receptors a vector of all the receptors to be checked. Defaults to all
#'   receptors exidting in DoOR.
#' @param responseRange the dataframe containing the info about the response
#'   ranges of all studies (\code{door_response_range})
#' @param weightGlobNorm the dataframe containing the info about the relative
#'   weights between receptors (\code{weight.globNorm})
#' @seealso \code{\link{import_new_data}}
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases removeStudy remove_study
#' @export
#' @examples
#' library(DoOR.data)
#' load_door_data()
#' remove_study('Bruyne.2001.WT')
#'
remove_study <- function(study,
                        receptors = door_default_values('ORs'),
                        responseRange = door_default_values('door_response_range'),
                        weightGlobNorm = door_default_values('weight.globNorm')) {
  if (length(study) > 1)
    stop('Please enter only 1 study at a time.')

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
    assign('door_response_range', responseRange, envir = .GlobalEnv)
    message(paste('removed',study,'from \'door_response_range\''))
  } else {
    warning(paste(study, 'not found in \'door_response_range\''))
  }

  if(study %in% names(weightGlobNorm)) {
    weightGlobNorm <- weightGlobNorm[, - which(colnames(weightGlobNorm) == study)]
    assign('weight.globNorm', weightGlobNorm, envir = .GlobalEnv)
    message(paste('removed',study,'from \'weight.globNorm\''))
  } else {
    warning(paste(study, 'not found in \'weight.globNorm\''))
  }
}
