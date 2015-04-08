default.val <-
function(DoOR_default)

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## return default values for DoOR functions

#  DoOR_default: character string

{

	## real number
	if (DoOR_default == "select.MDValue") 	{ return(0.1415) } # selectModel.R; modelRP.R; CreateDatabase.R; CreateDatabase.R
	if (DoOR_default == "points.cex") 	{ return(1) } # projectPoints.R
	if (DoOR_default == "cex.title") 	{ return(1) } # projectPoints.R
	if (DoOR_default == "dot.size") 	{ return(3) } # ORdotplot.R
	if (DoOR_default == "cex.labels") 	{ return(1) } # ORdotplot.R

	## integer
	if (DoOR_default == "overlapValues") 	{ return(5) } # selectModel.R
	if (DoOR_default == "num.charColumns") 	{ return(4) } # CreateDatabase.R, modelRP.R, importNewData.R, modelRPSEQ.R

	## logical
	if (DoOR_default == "glob.normalization") { return(TRUE) } # modelRP.R
	if (DoOR_default == "merged") 	       	  { return(TRUE) } # selectModel.R
	if (DoOR_default == "select.MD") 	  { return(TRUE) } # calModel.R; projectPoints.R
	if (DoOR_default == "closest") 		  { return(TRUE) } # LLSIest.R
	if (DoOR_default == "ind.panels") 	  { return(TRUE) } # ALimage.R
	if (DoOR_default == "title") 		  { return(FALSE) } # projectPoints.R
	if (DoOR_default == "plot") 		  { return(FALSE) } # modelRP.R; projectPoints.R

	## default null
	if (DoOR_default == "main") { return(NULL) } # ALimage.R

	## character
	if (DoOR_default == "zero") 		{ return("SFR") } # findRespNorm.R
	if (DoOR_default == "tag.ALimage") 	{ return("Glomerulus") } # ALimage.R
	if (DoOR_default == "tag") 		{ return("CAS") } # PlotChemicals.R; PlotReceptors.R; CreateDatabase.R; CreateDatabase.R
  if (DoOR_default == "ident") { return("InChIKey") } # importNewData.R

	## character vector
	if (DoOR_default == "col.extrem") 	{ return(c("blue", "red")) } # ALimage.R; ORdotplot.R; PlotChemicals.R; PlotReceptors.R
	if (DoOR_default == "interval.X") 	{ return(c(-1,2)) } # compute_MD.R; modelfunction.R; projectPoints.R

	## data frame
	if (DoOR_default == "AL256") 		{ data(AL256); return(AL256) } # ALimage.R
	if (DoOR_default == "data.format") 	{ data(data.format); return(data.format) } # importNewData.R
	if (DoOR_default == "odor.data") 	{ data(odor); return(odor) } # showOdor.R
	if (DoOR_default == "odor.dist") 	{ data(odor.dist); return(odor.dist) } # ALimage.R
	if (DoOR_default == "OGN") 		{ data(OGN); return(OGN) } # ALimage.R
	if (DoOR_default == "ORs") 		{ data(ORs); return(ORs) } # exportData.R; LoadRD.R; loadRDlist.R; importNewData.R
	if (DoOR_default == "response.matrix") 	{ data(response.matrix); return(response.matrix) } # exportData.R; findRespNorm.R
	if (DoOR_default == "response.range") 	{ data(response.range); return(response.range) } # exportData.R; findResp.R; importNewData.R; updateDatabase.R; modelRP.R
	if (DoOR_default == "unglobalNorm_response.matrix") { data(unglobalNorm_response.matrix); return(unglobalNorm_response.matrix) } # exportData.R
	if (DoOR_default == "weight.globNorm") 	{ data(weight.globNorm); return(weight.globNorm) } # exportData.R; importNewData.R; updateDatabase.R; modelRP.R

}
