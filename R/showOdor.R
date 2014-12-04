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
