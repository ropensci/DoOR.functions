#' Mapping the given receptor into response matrix
#' 
#' Mapping the given receptor into response matrix by comparing their linear
#' relationship (Pearson Correlation Coefficient).
#' 
#' 
#' @param da data frame; odorant response data for a given receptor, e.g.
#' Or22a.
#' @param by.column character string; specifying the column in "da" that
#' contains the response values.
#' @param Receptors character vector; containing the names of receptor that
#' specify the consensus values in response matrix
#' @param ResponseMatrix output is a numeric vector that contains the Pearson
#' Correlation Coefficient between given data and selected consensus data in
#' response matrix
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
mapReceptor <-
function(da, by.column, Receptors, ResponseMatrix) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# mapReceptor.R:
#################

# Mapping the given receptor into response matrix by comparing their linear relationship (Pearson Correlation Coefficient). 

# input parameters:
####################


# da 		 : data frame; odorant response data for a given receptor, e.g. Or22a. 
# by.column 	 : character string; specifying the column in "da" that contains the response values.
# Receptors 	 : character vector; containing the names of receptor that specify the consensus values in response matrix
# ResponseMatrix : a numeric matrix; containing the consensus response values.

# output is a numeric vector that contains the Pearson Correlation Coefficient between given data and selected consensus data in response matrix.
{
  res 		<- data.frame()
  matchOdor 	<- match(da[,"InChIKey"], rownames(ResponseMatrix))
  for (i in Receptors) {
    dataVector 	    <- da[,by.column]
    ResponseMatrixi <- ResponseMatrix[matchOdor,i]
    xy 		          <- na.omit(cbind(dataVector,ResponseMatrixi))
    # if no data available for selected receptor or no overlapped values with the selected receptor, then return NA and run next loop
    if (is.na(which(!is.na(ResponseMatrix[matchOdor,i]))[1]) | dim(xy)[1] < 3) {
      cor.coeff <- NA
      cor.pval  <- NA
    } else if (lm(ResponseMatrixi ~ dataVector)$coef[2] == 0 | is.na(lm(ResponseMatrixi ~ dataVector)$coef[2])) {
      # if the two data are fitted horizontally or vertically, then return NA and run next loop
      cor.coeff <- NA
      cor.pval  <- NA
    } else {
      correl <- cor.test(x = dataVector,y = ResponseMatrixi)
      cor.coeff <- correl$estimate
      cor.pval  <- correl$p.value
    }
    res.x <- data.frame(receptor = i, cor.coeff = cor.coeff, p.value = cor.pval, n = dim(xy)[1])
    res 	<- rbind(res,res.x)
    res   <- res[order(res$cor.coeff, decreasing = T, na.last = T),]
  }
  return(res)
}
