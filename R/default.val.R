#' default values for DoOR functions
#' 
#' \code{default.val} is used to return default values for DoOR functions.
#' 
#' There are six categories for default value. real number, integer, logical,
#' NULL, character string and character vector.
#' 
#' @param DoOR_default a character string; indicating which argument is to be
#' returned for DoOR functions.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @examples
#' 
#' default.val(DoOR_default = "select.MD")
#' 
default.val <- function(DoOR_default) {

	## real number
	if (DoOR_default == "select.MDValue") 	{ return(0.1415) } # selectModel.R; modelRP.R; CreateDatabase.R; CreateDatabase.R
	if (DoOR_default == "points.cex") 	    { return(1) } # projectPoints.R
	if (DoOR_default == "cex.title") 	      { return(1) } # projectPoints.R
	if (DoOR_default == "dot.size") 	      { return(3) } # ORdotplot.R
	if (DoOR_default == "cex.labels") 	    { return(1) } # ORdotplot.R

	## integer
	if (DoOR_default == "overlapValues") 	  { return(5) } # selectModel.R
	if (DoOR_default == "num.charColumns") 	{ return(5) } # CreateDatabase.R, modelRP.R, importNewData.R, modelRPSEQ.R

	## logical
	if (DoOR_default == "glob.normalization") { return(TRUE) } # modelRP.R
	if (DoOR_default == "merged") 	       	  { return(TRUE) } # selectModel.R
	if (DoOR_default == "select.MD") 	        { return(TRUE) } # calModel.R; projectPoints.R
	if (DoOR_default == "closest") 		        { return(TRUE) } # LLSIest.R
	if (DoOR_default == "scalebar")   	      { return(TRUE) } # ALimage.R
	if (DoOR_default == "title") 		          { return(FALSE) } # projectPoints.R
	if (DoOR_default == "plot") 		          { return(FALSE) } # modelRP.R; projectPoints.R

	## default null
	if (DoOR_default == "main") { return(NULL) } # ALimage.R

	## character
  if (DoOR_default == "zero") 		    { return("SFR") } # findRespNorm.R
  if (DoOR_default == "tag.ALimage") 	{ return("glomerulus") } # ALimage.R
  if (DoOR_default == "tag") 		      { return("InChIKey") } # PlotChemicals.R; PlotReceptors.R; CreateDatabase.R
  if (DoOR_default == "ident")        { return("InChIKey") } # importNewData.R

	## character vector
	if (DoOR_default == "col.extrem") 	{ return(c("blue", "red")) } # ALimage.R; ORdotplot.R; PlotChemicals.R; PlotReceptors.R
	if (DoOR_default == "interval.X") 	{ return(c(-1,2)) } # compute_MD.R; modelfunction.R; projectPoints.R

	## data frame
	if (DoOR_default == "AL256") {
    if (!exists("AL256")) {
      data(AL256)
    }
    return(AL256)
  } # ALimage.R
  
	if (DoOR_default == "data.format") {
	  if (!exists("data.format")) {
	    data(data.format)
	  }
	  return(data.format)
	} # importNewData.R

	if (DoOR_default == "odor.data") {
	  if (!exists("odor.data")) {
	    data(odor)
	  }
	  return(odor)
	} # showOdor.R
  
	if (DoOR_default == "odor.dist") {
	  if (!exists("odor.dist")) {
	    data(odor.dist)
	  }
	  return(odor.dist)
	} # ALimage.R
  
	if (DoOR_default == "weight.globNorm") {
	  if (!exists("weight.globNorm")) {
	    data(weight.globNorm)
	  }
	  return(weight.globNorm)
	} # exportData.R; importNewData.R; updateDatabase.R; modelRP.R

	if (DoOR_default == "DoOR.mappings") {
	  if (!exists("DoOR.mappings")) {
	    data(DoOR.mappings)
	  }
	  return(DoOR.mappings)
	} # ALimage.R
  
	if (DoOR_default == "ORs") {
	  if (!exists("ORs")) {
	    data(ORs)
	  }
	  return(ORs)
	} # exportData.R; LoadRD.R; loadRDlist.R; importNewData.R
  
	if (DoOR_default == "response.matrix") {
	  if (!exists("response.matrix")) {
	    data(response.matrix)
	  }
	  return(response.matrix)
	} # exportData.R; findRespNorm.R
  
	if (DoOR_default == "response.range") {
	  if (!exists("response.range")) {
	    data(response.range)
	  }
	  return(response.range)
	} # exportData.R; findResp.R; importNewData.R; updateDatabase.R; modelRP.R
  
	if (DoOR_default == "unglobalNorm_response.matrix") {
	  if (!exists("unglobalNorm_response.matrix")) {
	    data(unglobalNorm_response.matrix)
	  }
	  return(unglobalNorm_response.matrix)
	} # exportData.R

}
