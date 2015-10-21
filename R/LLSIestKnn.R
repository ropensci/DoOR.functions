# Local Least Squares Imputation Estimation using k-nearest neighbor
#
# Using local least squares imputation to estimate the missing values in
# target odorant receptors, odors will be selected by using k-nearest
# neighbor.
#
# The response matrix in common scale \code{respose.matrix} allows using local
# least squares imputation (LLS) to estimate the missing values in target
# odorant receptors. Odorant responses of most structural similar odors will
# be constructed as a linear combination for LLS. The odors will be selected
# by using k-nearest neighbor that have large absolute values of Pearson
# correlation coefficients. \code{nodor} indicates the number of selected
# odors.
#
# @param InChIKey a character string, the InChIKey of an odorant.
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
# LLSIestKnn(InChIKey = "MQWCXKGKQLNYQG-UHFFFAOYSA-N", receptor = "Or22a" )
#
#' @importFrom stats na.omit
LLSIestKnn <-
  function(InChIKey,
           receptor,
           response_matrix = default.val("response.matrix"),
           nodor = 3) {

    # part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
    # Neurobiology, University of Konstanz, Germany

    ### subfunction
    if (!requireNamespace("class", quietly = TRUE))
      stop("package 'class' is required, please install via install.packages('class')", call. = FALSE)

    nearest <- function (X, n, k=3)
      ## source code reference:  Hans Werner Borchers, http://tolstoy.newcastle.edu.au/R/help/04/02/0089.html
      ## Find k nearest neighbors of X[n, ] in the data frame
      ## or matrix X, utilizing function knn1 k-times.
    {
      N <- nrow(X)
      # inds contains the indices of nearest neighbors
      inds <- c(n)
      i <- 0
      while (i < k) {
        # use knn1 for one index...
        j <- as.integer(class::knn1(X [-inds, ], X[n, ], 1:(N-length(inds))))
        # ...and change to true index of neighbor
        inds <- c(inds, setdiff(1:N, inds)[j])
        i <- i+1
      }
      # return nearest neighbor indices (without n, of course)
      return(inds[-1])
    }

    available <- function(x) { length(which(!is.na(x))) }


    ## main program starts here


    # first: subtract the non-NA vectors (b, w) and matrix A to complete the linear combination
    #	[alfa 	w]
    #	[b    	A]
    # where "alfa" is the unknown odorant response, "w" is a vector of odorant responses of target odor, "b" is a vector of odorant responses of target receptor, after completing "b" and "w", the matrix "A" is formed.


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

    # the odors are not yet sorted.
    unsorted_b <- c( as.matrix( response_matrix[matchOdor, receptor] ) )

    ## sorting odors:

    if (missing(nodor)) { nodor = dim(candi_A)[1] }
    # find neighbors
    kNeighbors <- (nearest(X = rbind(w, candi_A), n = 1 , k = nodor) - 1)

    # matrix "A"
    A <- candi_A[kNeighbors,]

    # vector "b"
    b <- unsorted_b[kNeighbors]
    selectedOdor <- rownames(candi_A)[kNeighbors]

    # transpose matrix A
    if (dim(A)[1] == 1) { transp_A <- t(t(c(as.matrix(A)))) } # transpose from 1 x m to m x 1 matrix
    else { transp_A <- t(A) }

    alfa <- t(b) %*% PseudoInverse(transp_A) %*% as.matrix(w)

    result <- list(estimation = alfa,
                   selected.receptors = selectReceptor,
                   selected.odors = selectedOdor)

    return(result)
  }
