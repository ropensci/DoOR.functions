#' default values for DoOR functions
#'
#' \code{default.val} is used to return default values for DoOR functions.
#'
#' There are six categories for default value. real number, integer, logical,
#' NULL, character string and character vector.
#'
#' @param DoOR_default a character string, indicating which argument is to be
#' returned for DoOR functions.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @export
#' @importFrom utils data
#' @examples
#'
#' default.val(DoOR_default = "select.MD")
#'
default.val <- function(DoOR_default) {

  ## real number
  if (DoOR_default == "select.MDValue") 	{ return(0.1415) } # selectModel.R, modelRP.R, CreateDatabase.R, CreateDatabase.R
  if (DoOR_default == "points.cex") 	    { return(1) } # projectPoints.R
  if (DoOR_default == "cex.title") 	      { return(1) } # projectPoints.R
  if (DoOR_default == "dot.size") 	      { return(3) } # dplot_responseMatrix.R
  if (DoOR_default == "cex.labels") 	    { return(1) } # dplot_responseMatrix.R

  ## integer
  if (DoOR_default == "overlapValues") 	  { return(5) } # selectModel.R
  if (DoOR_default == "num.charColumns") 	{ return(5) } # CreateDatabase.R, modelRP.R, importNewData.R, modelRPSEQ.R

  ## logical
  if (DoOR_default == "glob.normalization") { return(TRUE) } # modelRP.R
  if (DoOR_default == "merged") 	       	  { return(TRUE) } # selectModel.R
  if (DoOR_default == "select.MD") 	        { return(TRUE) } # calModel.R, projectPoints.R
  if (DoOR_default == "closest") 		        { return(TRUE) } # LLSIest.R
  if (DoOR_default == "scalebar")   	      { return(TRUE) } # dplot_ALmap.R
  if (DoOR_default == "title") 		          { return(FALSE) } # projectPoints.R
  if (DoOR_default == "plot") 		          { return(FALSE) } # modelRP.R, projectPoints.R

  ## default null
  if (DoOR_default == "main") { return(NULL) } # dplot_ALmap.R

  ## character
  if (DoOR_default == "zero") 		    { return("SFR") } # getNormalizedResponses.R
  if (DoOR_default == "tag.ALmap") 	{ return("glomerulus") } # dplot_ALmap.R
  if (DoOR_default == "tag") 		      { return("InChIKey") } # dplot_responseProfile.R, dplot_compareReceptors.R, CreateDatabase.R
  if (DoOR_default == "ident")        { return("InChIKey") } # importNewData.R
  if (DoOR_default == "color.receptor") {return("#5A9BD4")}  # dplot_tuningCurveReceptor
  if (DoOR_default == "color.odorant")  {return("#7AC36A")}  # dplot_tuningCurveOdorant


  ## character vector
  if (DoOR_default == "colors")   { return(c("#0570b0","#74a9cf","#ffffff","#fdcc8a","#fc8d59","#d7301f")) } # dplot_ALmap.R, dplot_responseMatrix.R, dplot_responseProfile.R, dplot_compareReceptors.R
  if (DoOR_default == "interval.X") 	{ return(c(-1,2)) } # compute_MD.R, modelfunction.R, projectPoints.R

  ## data frame
  if (DoOR_default == "AL.map") {
    if (!exists("AL.map")) {
      data(AL.map)
    }
    return(AL.map)
  } # dplot_ALmap.R

  if (DoOR_default == "data.format") {
    if (!exists("data.format")) {
      data(data.format)
    }
    return(data.format)
  } # importNewData.R

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
  } # dplot_ALmap.R

  if (DoOR_default == "weight.globNorm") {
    if (!exists("weight.globNorm")) {
      data(weight.globNorm)
    }
    return(weight.globNorm)
  } # exportData.R, importNewData.R, updateDatabase.R, modelRP.R

  if (DoOR_default == "excluded.data") {
    if (!exists("excluded.data")) {
      data(excluded.data)
    }
    return(excluded.data)
  } # updateDatabase.R, modelRP.R, CreateDatabase.R


  if (DoOR_default == "DoOR.mappings") {
    if (!exists("DoOR.mappings")) {
      data(DoOR.mappings)
    }
    return(DoOR.mappings)
  } # dplot_ALmap.R

  if (DoOR_default == "ORs") {
    if (!exists("ORs")) {
      data(ORs)
    }
    return(ORs)
  } # exportData.R, load_door_data.R, load2list.R, importNewData.R

  if (DoOR_default == "response.matrix") {
    if (!exists("response.matrix")) {
      data(response.matrix)
    }
    return(response.matrix)
  } # exportData.R, getNormalizedResponses.R

  if (DoOR_default == "response.range") {
    if (!exists("response.range")) {
      data(response.range)
    }
    return(response.range)
  } # exportData.R, getResponses.R, importNewData.R, updateDatabase.R, modelRP.R

  if (DoOR_default == "response.matrix_non.normalized") {
    if (!exists("response.matrix_non.normalized")) {
      data(response.matrix_non.normalized)
    }
    return(response.matrix_non.normalized)
  } # exportData.R

}
