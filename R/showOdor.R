#' Link to PubChem
#' 
#' obtain information on a chemical from PubChem
#' 
#' Send a query to PubChem. The result is displayed using the default
#' WWW-browser.
#' 
#' @param CID a character string; the compound identity (CID) in pubchem.
#' @param CAS a character string; the CAS number.
#' @param odor.data data frame; chemical data of odors.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @seealso \code{\link{browseURL}}
#' @keywords data
#' @export
#' @examples
#' 
#' library(DoOR.data)
#' data(odor)
#' showOdor(CID=222,odor.data=odor)
#' showOdor(CAS="64-19-7",odor.data=odor)
#' 
showOdor <-
function(CID, CAS, odor.data = default.val("odor.data")) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# showOdor.R:
##############

# obtains information on a chemical from PubChem

#  CID 	     : a character string; the compound identity (CID) in PubChem.
#  CAS 	     : a character string; the CAS number.
#  odor.data : data frame; information on the chemical (formula, some descriptors) : can be accessed with "data(odor)"  from the R command line


# output: sends a query to PubChem. The result is displayed using the default WWW-browser.

{
	if (!missing(CID)) 
	{
		browseURL(paste("http://pubchem.ncbi.nlm.nih.gov/
			summary/summary.cgi?cid=",CID,"&loc=ec_rcs",sep=""))
	}

	if (!missing(CAS)) 
	{
		CID <- odor.data[odor.data$CAS==CAS,"CID"]
		browseURL(paste("http://pubchem.ncbi.nlm.nih.gov/
			summary/summary.cgi?cid=",CID,"&loc=ec_rcs",sep=""))

		if (is.na(CID[1])) { message("could not find the desired chemical in object 'odor'.") }
	}
}
