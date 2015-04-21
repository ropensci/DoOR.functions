#' Local Least Squares Imputation Estimation Using Pearson Correlation
#' 
#' Using local least squares imputation to estimate the missing values in
#' target odorant receptors, odors will be selected by coherent odors that have
#' large absolute values of Pearson correlation coefficients.
#' 
#' The response matrix in common scale \code{respose.matrix} allows using local
#' least squares imputation (LLS) to estimate the missing values in target
#' odorant receptors. Odorant responses of most structural similar odors will
#' be constructed as a linear combination for LLS. The odors will be selected
#' by using k-nearest neighbor that have large absolute values of Pearson
#' correlation coefficients. \code{nodor} indicates the number of selected
#' odors.
#' 
#' @param CAS a character string; the CAS number of odorant compound.
#' @param receptor a character string; the name of odorant receptor.
#' @param responseMatrix a numeric matrix; containing the normalized odorant
#' responses.
#' @param nodor a numeric value; specifying the number of the selected odors.
#' @return A list with components \code{estimation}, \code{selected.receptors}
#' and \code{selected.odors} which give the value of estimation, the selected
#' receptors and odors with absolute values of Pearson correlation coefficients
#' for linear combinations, respectively.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @references Kim, H.; Golub, G. H. & Park, H., Missing value estimation for
#' DNA microarray gene expression data: local least squares imputation., 2005,
#' Bioinformatics, 21, 187-198
#' @keywords math
#' @examples
#' 
#' library(DoOR.data)
#' LLSIestPC(CAS = "589-91-3", receptor = "Or22a" )
#' 
LLSIestPC <-
function(CAS, 
		receptor, 
		responseMatrix = default.val("response.matrix"),
		nodor = 2)
# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# LLSIestKnn.R:
##############

## Using local least squares imputation to estimate the missing values in target odorant receptors.

#  CAS 		   	: a character string; the CAS number of odorant compound.
#  receptor 	   	: a character string; the name of odorant receptor.
#  responseMatrix 	: a numeric matrix; containing the normalized odorant responses.
#  nodor 		: a numeric; specifying how many odors will be selected. If missing, all candicate odors will be selected after sorting.


## output: 	
#  estimation: value of estimation
#  selected.receptors: selected receptors
#  selected.odors: odors with absolute values of Pearson correlation coefficients

## reference: 
#  Hyunsoo Kim, Gene H. Golub and Haesun Park, 2005, Missing value estimation for DNA microarray gene expression data: local least squares imputation, Bioinformatics, Vol. 21, pages 187-198


{

	### subfunction
	## Find "k" nearest neighbors to "target" in the "candicate", utilizing function cor.test().
	nearest <- function (target, candicate, k)
	{
		N <- nrow(candicate)
		if (missing(k)) { k <- N }
		if (N < k)
		{
			message("The number of available odors is smaller than the default (3), so that only available odors will be selected.")
			k <- N 
		}
    		# absolute values of Pearson correlation coefficients between target and candicates
		absCorr 	<- abs(apply(candicate,1,function(x) cor.test(target,x)$estimate))
		sorted_absCorr 	<- sort(absCorr, decreasing = TRUE)

    		return(sorted_absCorr[1:k])
	}


	available <- function(x) { length(which(!is.na(x))) }

	## main program starts here

	# first: subtract the non-NA vectors (b, w) and matrix A to complete the linear combination 
	#	[alfa 	w]
	#	[b    	A]
	# where "alfa" is the unknown odorant response, "w" is a vector of odorant responses of target odor, "b" is a vector of odorant responses of target receptor, after completing "b" and "w", the matrix "A" is formed.

	responseMatrix 		<- as.data.frame(responseMatrix)
	# localize the target receptor and odor in sorted response matrix
	whereTargetReceptor 	<- match(receptor,colnames(responseMatrix))
	whereTargetodor 	<- match(CAS,rownames(responseMatrix))

	# non-NA vectors (b ("candicateOdors") and w ("candicateReceptors") ) as candicates
	candicateReceptors 	<- which(!is.na(responseMatrix[whereTargetodor,]))
	Name_candicateReceptors <- colnames(responseMatrix)[candicateReceptors]
	candicateOdors 		<- which(!is.na(responseMatrix[,whereTargetReceptor]))
	Name_candicateOdors 	<- rownames(responseMatrix)[candicateOdors]


	# omit the columns and rows that contain NA

	candi_A <- na.omit( responseMatrix[candicateOdors, candicateReceptors])

	# detect the posistions of selected odors and receptors
	matchOdor <- match( rownames(candi_A),rownames(responseMatrix) )
	matchReceptor <- match( colnames(candi_A), colnames(responseMatrix) )

	# vector "w"
	w <- c( as.matrix( responseMatrix[CAS, matchReceptor] ) )
	selectReceptor <- names(responseMatrix[CAS, matchReceptor])

	# find neighbors
	kNeighbors <- nearest(target = w, candicate = candi_A, k = nodor)

	selectedOdor <- names(kNeighbors)

	# matrix "A"
	A <- candi_A[selectedOdor,]

	# vector "b"
	b <- responseMatrix[selectedOdor, receptor]
	
	# transpose matrix A
	if (dim(A)[1] == 1) { transp_A <- t(t(c(as.matrix(A)))) } # transpose from 1 x m to m x 1 matrix
	else { transp_A <- t(A) }

	alfa <- t(b) %*% PseudoInverse(transp_A) %*% as.matrix(w)

	result <- list(estimation = alfa,
			selected.receptors = selectReceptor,
			selected.odors = kNeighbors)

	return(result)
}
