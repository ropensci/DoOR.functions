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
  if (DoOR_default == "select.MDValue") 	{ return(0.1415) } # select_model.R, model_response.R, create_door_database.R, create_door_database.R
  if (DoOR_default == "points.cex") 	    { return(1) } # project_points.R
  if (DoOR_default == "cex.title") 	      { return(1) } # project_points.R
  if (DoOR_default == "dot.size") 	      { return(3) } # dplot_response_matrix.R
  if (DoOR_default == "cex.labels") 	    { return(1) } # dplot_response_matrix.R

  ## integer
  if (DoOR_default == "overlapValues") 	  { return(5) } # select_model.R
  if (DoOR_default == "num.charColumns") 	{ return(5) } # create_door_database.R, model_response.R, import_new_data.R, model_response_seq.R

  ## logical
  if (DoOR_default == "glob.normalization") { return(TRUE) } # model_response.R
  if (DoOR_default == "merged") 	       	  { return(TRUE) } # select_model.R
  if (DoOR_default == "select.MD") 	        { return(TRUE) } # calculate_model.R, project_points.R
  if (DoOR_default == "closest") 		        { return(TRUE) } # LLSIest.R
  if (DoOR_default == "scalebar")   	      { return(TRUE) } # dplot_al_map.R
  if (DoOR_default == "title") 		          { return(FALSE) } # project_points.R
  if (DoOR_default == "plot") 		          { return(FALSE) } # model_response.R, project_points.R

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
  if (DoOR_default == "interval.X") 	{ return(c(-1,2)) } # compute_MD.R, modelfunction.R, project_points.R

  ## data frame
  if (DoOR_default == "door_AL_map") {
    if (!exists("door_AL_map")) {
      data(door_AL_map)
    }
    return(door_AL_map)
  } # dplot_al_map.R

  if (DoOR_default == "door_data_format") {
    if (!exists("door_data_format")) {
      data(door_data_format)
    }
    return(door_data_format)
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

  if (DoOR_default == "door_global_normalization_weights") {
    if (!exists("door_global_normalization_weights")) {
      data(door_global_normalization_weights)
    }
    return(door_global_normalization_weights)
  } # export_door_data.R, import_new_data.R, update_database.R, model_response.R

  if (DoOR_default == "door_excluded_data") {
    if (!exists("door_excluded_data")) {
      data(door_excluded_data)
    }
    return(door_excluded_data)
  } # update_database.R, model_response.R, create_door_database.R


  if (DoOR_default == "door_mappings") {
    if (!exists("door_mappings")) {
      data(door_mappings)
    }
    return(door_mappings)
  } # dplot_al_map.R

  if (DoOR_default == "ORs") {
    if (!exists("ORs")) {
      data(ORs)
    }
    return(ORs)
  } # export_door_data.R, load_door_data.R, load2list.R, import_new_data.R

  if (DoOR_default == "door_response_matrix") {
    if (!exists("door_response_matrix")) {
      data(door_response_matrix)
    }
    return(door_response_matrix)
  } # export_door_data.R, get_normalized_responses.R

  if (DoOR_default == "door_response_range") {
    if (!exists("door_response_range")) {
      data(door_response_range)
    }
    return(door_response_range)
  } # export_door_data.R, get_responses.R, import_new_data.R, update_database.R, model_response.R

  if (DoOR_default == "door_response_matrix_non_normalized") {
    if (!exists("door_response_matrix_non_normalized")) {
      data(door_response_matrix_non_normalized)
    }
    return(door_response_matrix_non_normalized)
  } # export_door_data.R

}
