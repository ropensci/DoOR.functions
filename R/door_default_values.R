#' default values for DoOR functions
#'
#' \code{door_default_values} is used to return default values for DoOR functions.
#'
#' There are six categories for default value. real number, integer, logical,
#' NULL, character string and character vector.
#'
#' @param DoOR_default a character string, indicating which argument is to be
#' returned for DoOR functions.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @aliases default.val door_default_values
#' @keywords data
#' @export
#' @importFrom utils data
#' @examples
#'
#' door_default_values(DoOR_default = "select.MD")
#'
door_default_values <- function(DoOR_default) {

  ## real number
  if (DoOR_default == "select.MDValue") 	{ return(0.1415) } # selectModel.R, modelRP.R, create_door_database.R, create_door_database.R
  if (DoOR_default == "points.cex") 	    { return(1) } # projectPoints.R
  if (DoOR_default == "cex.title") 	      { return(1) } # projectPoints.R
  if (DoOR_default == "dot.size") 	      { return(3) } # dplot_response_matrix.R
  if (DoOR_default == "cex.labels") 	    { return(1) } # dplot_response_matrix.R

  ## integer
  if (DoOR_default == "overlapValues") 	  { return(5) } # selectModel.R
  if (DoOR_default == "num.charColumns") 	{ return(5) } # create_door_database.R, modelRP.R, import_new_data.R, modelRPSEQ.R

  ## logical
  if (DoOR_default == "glob.normalization") { return(TRUE) } # modelRP.R
  if (DoOR_default == "merged") 	       	  { return(TRUE) } # selectModel.R
  if (DoOR_default == "select.MD") 	        { return(TRUE) } # calculate_model.R, projectPoints.R
  if (DoOR_default == "closest") 		        { return(TRUE) } # LLSIest.R
  if (DoOR_default == "scalebar")   	      { return(TRUE) } # dplot_al_map.R
  if (DoOR_default == "title") 		          { return(FALSE) } # projectPoints.R
  if (DoOR_default == "plot") 		          { return(FALSE) } # modelRP.R, projectPoints.R

  ## default null
  if (DoOR_default == "main") { return(NULL) } # dplot_al_map.R

  ## character
  if (DoOR_default == "zero") 		    { return("SFR") } # get_normalized_responses.R
  if (DoOR_default == "tag.ALmap") 	{ return("glomerulus") } # dplot_al_map.R
  if (DoOR_default == "tag") 		      { return("InChIKey") } # dplot_response_profile.R, dplot_compareReceptors.R, create_door_database.R
  if (DoOR_default == "ident")        { return("InChIKey") } # import_new_data.R
  if (DoOR_default == "color.receptor") {return("#5A9BD4")}  # dplot_tuningCurveReceptor
  if (DoOR_default == "color.odorant")  {return("#7AC36A")}  # dplot_tuningCurveOdorant


  ## character vector
  if (DoOR_default == "colors")   { return(c("#0570b0","#74a9cf","#ffffff","#fdcc8a","#fc8d59","#d7301f")) } # dplot_al_map.R, dplot_response_matrix.R, dplot_response_profile.R, dplot_compareReceptors.R
  if (DoOR_default == "interval.X") 	{ return(c(-1,2)) } # compute_MD.R, modelfunction.R, projectPoints.R

  ## data frame
  if (DoOR_default == "AL.map") {
    if (!exists("AL.map")) {
      data(AL.map)
    }
    return(AL.map)
  } # dplot_al_map.R

  if (DoOR_default == "data.format") {
    if (!exists("data.format")) {
      data(data.format)
    }
    return(data.format)
  } # import_new_data.R

  if (DoOR_default == "odor") {
    if (!exists("odor")) {
      data(odor)
    }
    return(odor)
  }

  if (DoOR_default == "odor.dist") {
    if (!exists("odor.dist")) {
      data(odor.dist)
    }
    return(odor.dist)
  } # dplot_al_map.R

  if (DoOR_default == "weight.globNorm") {
    if (!exists("weight.globNorm")) {
      data(weight.globNorm)
    }
    return(weight.globNorm)
  } # export_door_data.R, import_new_data.R, updateDatabase.R, modelRP.R

  if (DoOR_default == "excluded.data") {
    if (!exists("excluded.data")) {
      data(excluded.data)
    }
    return(excluded.data)
  } # updateDatabase.R, modelRP.R, create_door_database.R


  if (DoOR_default == "DoOR.mappings") {
    if (!exists("DoOR.mappings")) {
      data(DoOR.mappings)
    }
    return(DoOR.mappings)
  } # dplot_al_map.R

  if (DoOR_default == "ORs") {
    if (!exists("ORs")) {
      data(ORs)
    }
    return(ORs)
  } # export_door_data.R, load_door_data.R, load2list.R, import_new_data.R

  if (DoOR_default == "response.matrix") {
    if (!exists("response.matrix")) {
      data(response.matrix)
    }
    return(response.matrix)
  } # export_door_data.R, get_normalized_responses.R

  if (DoOR_default == "response.range") {
    if (!exists("response.range")) {
      data(response.range)
    }
    return(response.range)
  } # export_door_data.R, get_responses.R, import_new_data.R, updateDatabase.R, modelRP.R

  if (DoOR_default == "response.matrix_non.normalized") {
    if (!exists("response.matrix_non.normalized")) {
      data(response.matrix_non.normalized)
    }
    return(response.matrix_non.normalized)
  } # export_door_data.R

}
