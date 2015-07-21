# calculate all possible permutations
#
# @param n length of vector to permute
#
permutations <- function(n) { # code from http://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
  if(n==1) {
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}
