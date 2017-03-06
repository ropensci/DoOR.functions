#'' select the best model function
#'
#'\code{calculate_model} is used to return the best model function that represent the
#'relationship between responses from study x and y.
#'
#'\code{calculate_model} chooses the best model function from following: linear,
#'exponential function, sigmoid, asymptotic model with x intercept, asympototic
#'model with y intercept and their inverse versions. (If your are interested in
#'these functions please check the sources at
#'https://github.com/Dahaniel/DoOR.functions)
#'
#'
#'@param x,y data vectors from study x and y (can contain NA)
#'@param select.MD logical, if TRUE, only the best model function (in terms of
#'  MD) will be returned.
#'@author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#'@author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#'@aliases calModel calculate_model
#'@export
#'@importFrom stats na.omit
#'
#'@examples
#' # load a data set 
#' library(DoOR.data)
#' data(Or35a)
#' 
#' # pick 2 data sets for Or35a and rescale the data [0,1]
#' x <- door_norm(Or35a[,6])
#' y <- door_norm(Or35a[,9])
#' # run calculate_model
#' calM_xy <- calculate_model(x, y, select.MD = door_default_values("select.MD"))
#'
calculate_model <- function (x, y, select.MD = door_default_values("select.MD") ) {

  comb.xy     <-  na.omit(cbind(x, y))
  if (dim(comb.xy)[1]==0) { stop("There is no observation between x and y.") }

  # compute the parameters of 5 optional models with and without inverse function (see modelfunction.R).
  models 	<- modelfunction(x,y)

  # select the best model from the ones tried above:

  if (select.MD == FALSE) {
    return(models)
  }

  if (select.MD == TRUE) {
    collect.MD <- sapply(models, "[[", "MD")
    if (all(is.na(collect.MD))) {
      return(NA)
      message("No model could be fitted")
    } else {
      return(models[match(min(collect.MD,na.rm=TRUE),collect.MD)])
    }
  }
}
