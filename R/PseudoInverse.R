PseudoInverse <-
function(x) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

## Evaluate the pseudoinvese of a matrix
# The pseudoinverse of the matrix x with singular value decomposition x = U %*% D %*% V^' is:
# x^+ = V %*% D^+ %*% U^'
# where D^+ is the pseudoinverse of D, which is formed by replacing every nonzero entry by its reciprocal.

#  x: a numeric matrix

{
	svd_x <- svd(x)
	reciprocal_D <- matrix(rep(0, times = length(svd_x$d)^2), nrow = length(svd_x$d))
	diag(reciprocal_D) <- svd_x$d^-1
	return(svd_x$v %*% reciprocal_D %*%  t(svd_x$u))
}
