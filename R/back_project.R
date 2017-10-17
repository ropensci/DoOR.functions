#' back_project
#'
#' project the model response values back into your scale of interest (spike,
#' deltaF/F...)
#'
#' The process of back projection is the following: \itemize{ \item 1. rescale
#' both data sets to [0,1], \item 2. find the best fitting model between
#' "bp.data" and "cons.data" (lowest MD value), \item 3. project the consensus
#' data onto the fitted curv, these are now our normalized, back projected
#' responses \item 4. rescale all responses to the scale of the original data
#' via a linear fit.  }
#'
#' @param template.data data frame, the data that provides the scale to
#'   transform to, containing InChIKeys in a column called "odorants" and
#'   responses in a column called "responses"
#' @param responding.unit character, the name of the receptor/OSN/glomerulus
#'   which responses should be transformed
#' @param response_matrix DoOR response mnatrix, the source data is picked from
#'   here
#'
#' @return Output of back_project is a list containing a data frame with the
#'   back_projected data, the original data, the data used as a template and the
#'   data that resulted from fitting source and template (before rescaling to
#'   the template scale). additionaly the parameters of the linear fit between
#'   the source and template response scale is returned.
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @aliases back_project back_project
#' @export
#' @importFrom stats lm
#' @importFrom graphics points
#'
#' @examples
#' # load some data sets
#' data(Or22a)
#' data(door_response_matrix)
#' 
#' # create example data we are going to back project
#' template.data <- data.frame(odorants = Or22a$InChIKey,
#'                             responses = Or22a$Hallem.2004.EN)
#'                             
#' # run back_project and plot the results                             
#' bp <- back_project(template.data, "Or22a")
#'
#' plot(bp$back_projected$original.data,
#'      bp$back_projected$back_projected.data,
#'      xlab = "DoOR consensus response",
#'      ylab = "back_projected data [spikes, Hallem.2004.EN]"
#' )
#'
back_project <- function(template.data,
                         responding.unit,
                         response_matrix = 
                           door_default_values("door_response_matrix")) {
  template.data$odorants <- as.character(template.data$odorants)
  source.data <-
    data.frame(
      odorants = rownames(response_matrix),
      cons.responses = response_matrix[, responding.unit],
      stringsAsFactors = F
    )
  
  # combine the data
  match.st <- match(source.data$odorants, template.data$odorants)
  xy       <- cbind(door_norm(source.data$cons.responses),
                    door_norm(template.data$responses[match.st]))
  
  # find the best fitting model and project all points along the fitted curve
  best.model <- calculate_model(x = xy[, 1], y = xy[, 2])
  projected  <-
    project_points(
      x = xy[, 1],
      y = xy[, 2],
      best.model = best.model,
      plot = TRUE,
      title = TRUE,
      xlab = paste(responding.unit, "[consensus data]"),
      ylab = "template data"
    )
  
  # extract the projected and the normalized projected data
  projected.back     <-
    rbind(projected$double.observations[, c('ID', 'Y')],
          projected$single.observations[, c('ID', 'Y')])
  projected.back_NDR <-
    rbind(projected$double.observations[, c('ID', 'NDR')],
          projected$single.observations[, c('ID', 'NDR')])
  
  # extract normalized and original values of the double observations
  # (points/odorants that appear in both data sets)
  double.observations_ID   <-
    projected$double.observations[, 'ID'] # IDs
  double.observations_norm <-
    projected$double.observations[, 'y']  # normalized values
  double.observations_orig <-
    template.data[double.observations_ID, 'responses'] # original values
  
  # perform a linear fit for "translation" of the consensus scale into the
  # template scale
  translate       <-
    lm(double.observations_orig ~ double.observations_norm)
  translate.parms <- c(translate$coef[1], translate$coef[2])
  
  # now rescale the whole data set according to these parameters
  match.sp <- match(1:dim(source.data)[1], projected.back[, 'ID'])
  projected.back_rescaled <-
    translate.parms[1] + translate.parms[2] * projected.back[match.sp, 'Y']
  
  back.projected <- list(
    back_projected =
      data.frame(
        odorants           = source.data$odorants,
        back_projected.data = projected.back_rescaled,
        original.data      = source.data$cons.responses,
        template.data      = template.data$responses[match.st],
        fitted.data        = projected.back_NDR[match.sp, "NDR"]
      ),
    rescale.function =
      c(
        "intercept" = as.vector(translate.parms[1]),
        "slope"     = as.vector(translate.parms[2])
      )
  )
  return(back.projected)
}
