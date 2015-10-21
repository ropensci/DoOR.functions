# Local Least Squares Imputation Estimation Using Pearson Correlation
#
# Using local least squares imputation to estimate the missing values in
# target odorant receptors, odors will be selected by coherent odors that have
# large absolute values of Pearson correlation coefficients.
#
# The response matrix in common scale \code{respose.matrix} allows using local
# least squares imputation (LLS) to estimate the missing values in target
# odorant receptors. Odorant responses of most structural similar odors will
# be constructed as a linear combination for LLS. The odors will be selected
# by using k-nearest neighbor that have large absolute values of Pearson
# correlation coefficients. \code{nodor} indicates the number of selected
# odors.
#
# @param InChIKey a character string, the InChIKey number of odorant compound.
# @param receptor a character string, the name of odorant receptor.
# @param response_matrix a numeric matrix, containing the normalized odorant
# responses.
# @param nodor a numeric value, specifying the number of the selected odors.
# @return A list with components \code{estimation}, \code{selected.receptors}
# and \code{selected.odors} which give the value of estimation, the selected
# receptors and odors with absolute values of Pearson correlation coefficients
# for linear combinations, respectively.
# @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
# @references Kim, H., Golub, G. H. & Park, H., Missing value estimation for
# DNA microarray gene expression data: local least squares imputation., 2005,
# Bioinformatics, 21, 187-198
# @keywords math
# @examples
#
# library(DoOR.data)
# LLSIestPC(InChIKey = "MQWCXKGKQLNYQG-UHFFFAOYSA-N", receptor = "Or22a" )
#
#' @importFrom stats na.omit cor.test
LLSIestPC <-
  function(InChIKey,
           receptor,
           response_matrix = default.val("response.matrix"),
           nodor = 3) {

    ### subfunction
    ## Find "k" nearest neighbors to "target" in the "candicate", utilizing function cor.test().
    nearest <- function (target, candicate, k) {
      N <- nrow(candicate)
      if (missing(k)) { k <- N }
      if (N < k) {
        message(paste0("The number of available odors is smaller than the minimum (",k,"), so that only available odors will be selected."))
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

    response_matrix 		<- as.data.frame(response_matrix)
    # localize the target receptor and odor in sorted response matrix
    whereTargetReceptor 	<- match(receptor,colnames(response_matrix))
    whereTargetodor 	<- match(InChIKey,rownames(response_matrix))

    # non-NA vectors (b ("candicateOdors") and w ("candicateReceptors") ) as candicates
    candicateReceptors 	<- which(!is.na(response_matrix[whereTargetodor,]))
    Name_candicateReceptors <- colnames(response_matrix)[candicateReceptors]
    candicateOdors 		<- which(!is.na(response_matrix[,whereTargetReceptor]))
    Name_candicateOdors 	<- rownames(response_matrix)[candicateOdors]


    # omit the columns and rows that contain NA

    candi_A <- na.omit( response_matrix[candicateOdors, candicateReceptors])

    # detect the posistions of selected odors and receptors
    matchOdor <- match( rownames(candi_A),rownames(response_matrix) )
    matchReceptor <- match( colnames(candi_A), colnames(response_matrix) )

    # vector "w"
    w <- c( as.matrix( response_matrix[InChIKey, matchReceptor] ) )
    selectReceptor <- names(response_matrix[InChIKey, matchReceptor])

    # find neighbors
    kNeighbors <- nearest(target = w, candicate = candi_A, k = nodor)

    selectedOdor <- names(kNeighbors)

    # matrix "A"
    A <- candi_A[selectedOdor,]

    # vector "b"
    b <- response_matrix[selectedOdor, receptor]

    # transpose matrix A
    if (dim(A)[1] == 1) { transp_A <- t(t(c(as.matrix(A)))) } # transpose from 1 x m to m x 1 matrix
    else { transp_A <- t(A) }

    alfa <- t(b) %*% PseudoInverse(transp_A) %*% as.matrix(w)

    result <- list(estimation = alfa,
                   selected.receptors = selectReceptor,
                   selected.odors = kNeighbors)

    return(result)
  }
