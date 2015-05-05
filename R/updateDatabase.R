#' update response matrix
#' 
#' update the globally \code{response matrix} and the unglobally normalized
#' response matrix \code{unglobalNorm_response.matrix} by introducing new
#' consensus response data of given receptor.
#' 
#' The merging sequence could be arranged by the routine process (using
#' \code{\link{modelRP}} or taking the optimized sequence that is chosen from
#' permutations. The mean correlation between merged responses and each
#' original recording will be computed for each permutation, the optimozed
#' sequence is with the highest correlation.
#' 
#' @param receptor character string; name of given odorant receptor.
#' @param permutation logical; if TRUE, the sequence is chosen from
#' permutation, if FALSE, sequence is chosen by the routine process.
#' @param unglobalNorm_responseMatrix data frame; response data that has not
#' been globally normalized.
#' @param responseMatrix data frame; globally normalized response data.
#' @param responseRange data frame; response range of studies.
#' @param weightGlobNorm data frame; weight matrix for global normalization.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @seealso \code{\link{modelRP}},\code{\link{modelRPSEQ}},
#' \code{\link[gregmisc]{permutations}}
#' @keywords data
#' @examples
#' 
#' library(DoOR.data)
#' loadRD()
#' # update the entry "Or67b" of data "response.matrix" and "unglobalNorm_response.matrix" with permutations.
#' # updateDatabase(receptor="Or67b", permutation = TRUE)
#' 
updateDatabase <-
function(receptor, permutation = TRUE, unglobalNorm_responseMatrix = default.val("unglobalNorm_response.matrix"), responseMatrix = default.val("response.matrix"), 
		responseRange = default.val("response.range"), weightGlobNorm = default.val("weight.globNorm"))

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# updateDatabase.R:
#####################


# updates the globally ("response matrix") and the locally normalized response matrix ("unglobalNorm_response.matrix")
# for a given receptor, i.e. applies the DoOR merging process of all available studies for the given receptor.


# input parameters:
#####################

# receptor 	 : character string; name of the receptor to update

# permutation 	 : logical; 
#                  if TRUE, the optimal merging sequence is chosen by exhaustive enumeration of sequences; 
#                  the optimal sequence is the one that leads to the highest mean correlation of all individual studies to the model response;
#
#                  if FALSE, we follow the heuristic approach for determining a good sequence (as described in the DoOR paper)

# unglobalNorm_responseMatrix   : data frame; response data that has not been globally normalized.
# responseMatrix 		: data frame; globally normalized response data.
# responseRange  : data frame; response range of studies.
# weightGlobNorm : data frame; weight matrix for global normalization.

## requires: library(gtools)

{	

	# main function starts here
	da 		<- get(receptor)
	recordColumn 	<- as.numeric( c((default.val("num.charColumns")+1):dim(da)[2]) )

	if (permutation == TRUE)
	{
		message("requires: library(gtools)")
		perm 	<- permutations(length(recordColumn), length(recordColumn),v=names(da)[recordColumn]) # implement faster version from http://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
		message(paste("All possible permutations (", dim(perm)[1], ") have been calculated, now merging.", sep = ""))
		# compute the mean correlation between merged responses and original response.
		meanCorrel <- matrix(NA,nrow=dim(perm)[1])

  for (i in 1:dim(perm)[1]) {
    tryMerg <- try(modelRPSEQ(data=da,SEQ=perm[i,]),silent = TRUE)
    if (inherits(tryMerg, "try-error")) { 
      meanCorrel_tryMerg <- NA 
      } else {
        meanCorrel_tryMerg <- mean(sapply(da[,recordColumn],function(x) calModel(tryMerg,x)[[1]]$MD),na.rm=TRUE) 
        }
    meanCorrel[i,] <- meanCorrel_tryMerg
    
    message(paste("[",i,"/",dim(perm)[1],"] ",paste(perm[i,], collapse = ", "), " ------ Mean distance: ", round(meanCorrel_tryMerg, 4), sep = ""))
  }
		
		message("--------------------------------------------------------")

  perm_MC <- data.frame(perm,meanCorrel) 	# data frame, the last column contains the mean correlation values.

	# find and show the sequence with the lowest MD
  min.MD  <- which.min(meanCorrel)
  SEQ     <- perm[min.MD,]
  
  print(paste("The optimized sequence with the lowest mean MD", round(meanCorrel[min.MD], 4), "is:"))
  print(SEQ)

	# merge response data with the optimized sequence.
	merg <- modelRPSEQ(data = da, SEQ = SEQ)

	} else { # END if (permutation == TRUE)
		merg <- modelRP(da, glob.normalization = FALSE)$model.response[,"merged_data"]
	}
	# update  unglobalNorm_response.matrix

	merged_data_withInChIKey <- data.frame(InChIKey = da$InChIKey, merged_data = merg)
	matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(unglobalNorm_responseMatrix))
	findNA_InChIKey <- which(is.na(matchInChIKey))
	if (!is.na(findNA_InChIKey[1])){
		addRow <- matrix(NA, nrow=length(findNA_InChIKey), ncol= dim(unglobalNorm_responseMatrix)[2])
		colnames(addRow) <- colnames(unglobalNorm_responseMatrix)
		rownames(addRow) <- merged_data_withInChIKey[findNA_InChIKey,"InChIKey"]
		unglobalNorm_responseMatrix <- rbind(unglobalNorm_responseMatrix,addRow)
		responseMatrix <- rbind(responseMatrix,addRow)
		matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(unglobalNorm_responseMatrix))
	}
	unglobalNorm_responseMatrix[matchInChIKey, receptor] <- merg
	assign("unglobalNorm_response.matrix", unglobalNorm_responseMatrix, envir = .GlobalEnv)


	# update response.matrix

  name.Stud    <- colnames(da)[recordColumn]
  mp_orx       <- match(colnames(da)[recordColumn], responseRange[,"study"])
  Rmax         <- apply(as.matrix(da[,recordColumn]),2,function(x) max(range(x,na.rm=TRUE)))
  Smax         <- responseRange[mp_orx,"max"]
  merged_data  <- globalNorm(RMAX = Rmax,SMAX = Smax, MV = merg, name.Stud = name.Stud, responseRange = responseRange, weightGlobNorm = weightGlobNorm)
  merged_data_withInChIKey <- data.frame(InChIKey = da$InChIKey, merged_data = merged_data)
  matchInChIKey <- match(merged_data_withInChIKey$InChIKey,rownames(responseMatrix))
  responseMatrix[matchInChIKey, receptor] <- merged_data

	assign("response.matrix", responseMatrix, envir = .GlobalEnv)
}
