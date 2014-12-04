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
		perm 	<- permutations(length(recordColumn), length(recordColumn),v=names(da)[recordColumn])
		
		# compute the mean correlation between merged responses and original response.
		meanCorrel <- matrix(NA,nrow=dim(perm)[1])

		for (i in 1:dim(perm)[1]) 
                {
			tryMerg <- try(modelRPSEQ(data=da,SEQ=perm[i,]),silent = TRUE)
			
                        if (inherits(tryMerg, "try-error")) { meanCorrel_tryMerg <- NA }
			else 
                        { 
				meanCorrel_tryMerg <- mean(apply(da[,recordColumn],2,function(x) calModel(tryMerg,x)[[1]]$MD),na.rm=TRUE) 
			}

			meanCorrel[i,] <- meanCorrel_tryMerg

			print("The permutated sequence:",justify="left")
			cat(perm[i,])
			cat("Mean distance:")
			print(round(meanCorrel_tryMerg, 4))
			
		}
		
		cat("--------------------------------------------------------")

        	perm_MC <- data.frame(perm,meanCorrel) 	# data frame, the last column contains the mean correlation values.

		# find and show the sequence with the highest correlation
	
           	sort_perm_MC <- perm_MC[order(perm_MC[,"meanCorrel"],decreasing=FALSE),]
		SEQ <- c(as.matrix(perm_MC[1,c(1:(dim(perm_MC)[2]-1))]))
		print(paste("The optimized sequence with the lowest mean MD", round(sort_perm_MC[1,"meanCorrel"], 4), "is:"))
		print(SEQ)

		# merge response data with the optimized sequence.
		merg <- modelRPSEQ(data = da, SEQ = SEQ)

	} # END if (permutation == TRUE)
	else
	{
		merg <- modelRP(da, glob.normalization = FALSE)$model.response[,"merged_data"]
	}


	# update  unglobalNorm_response.matrix

	merged_data_withCAS <- data.frame(CAS = da$CAS, merged_data = merg)
	matchCAS <- match(merged_data_withCAS$CAS,rownames(unglobalNorm_responseMatrix))
	findNA_CAS <- which(is.na(matchCAS))
	if (!is.na(findNA_CAS[1]))
	{
		addRow <- matrix(NA, nrow=length(findNA_CAS), ncol= dim(unglobalNorm_responseMatrix)[2])
		colnames(addRow) <- colnames(unglobalNorm_responseMatrix)
		rownames(addRow) <- merged_data_withCAS[findNA_CAS,"CAS"]
		unglobalNorm_responseMatrix <- rbind(unglobalNorm_responseMatrix,addRow)
		responseMatrix <- rbind(responseMatrix,addRow)
		matchCAS <- match(merged_data_withCAS$CAS,rownames(unglobalNorm_responseMatrix))
	}
	unglobalNorm_responseMatrix[matchCAS, receptor] <- merg
	assign("unglobalNorm_response.matrix", unglobalNorm_responseMatrix, envir = .GlobalEnv)


	# update response.matrix

	name.Stud    <- colnames(da)[recordColumn]
	mp_orx       <- match(colnames(da)[recordColumn], responseRange[,"study"])
        Rmax         <- apply(as.matrix(da[,recordColumn]),2,function(x) max(range(x,na.rm=TRUE)))
	Smax         <- responseRange[mp_orx,"max"]
	merged_data  <- globalNorm(RMAX = Rmax,SMAX = Smax, MV = merg, name.Stud = name.Stud, 
				responseRange = responseRange, weightGlobNorm = weightGlobNorm)
	merged_data_withCAS <- data.frame(CAS = da$CAS, merged_data = merged_data)
	matchCAS <- match(merged_data_withCAS$CAS,rownames(responseMatrix))
	responseMatrix[matchCAS, receptor] <- merged_data

	assign("response.matrix", responseMatrix, envir = .GlobalEnv)
}
