# pyramid
# 
# returns a "pyramidal" sequence with the length supplied by n
#
# @param n length of the sequence to be returned
#
# @return a "pyramidally ordered" sequence of integers with length n
#
# @examples pyramid(10)
pyramid <- function(n) {
  seq <- c(seq(1,n,2),rev(seq(2,n,2)))
  return(seq)
}

# orderPyramid
# 
# returns the "pyramidal" order for a vector with numeric values
#
# @param x vector that should be ordered
# @param na.rm whether ornot to remove missing values
#
# @return integer, the "pyramidal" order of x
#
# @examples
# data  <- rnorm(n = 20, mean = 2, sd = .2)
# order.pyramid <- orderPyramid(data)
# plot(data)
# plot(data[order.pyramid])
orderPyramid <- function(x, na.rm = F) {
  order.x   <- order(x)
  if(na.rm == T)
    order.x <- order.x[- which(order.x %in% which(is.na(x)))]
  length.x  <- length(order.x)
  pyramid.x <- order.x[pyramid(length.x)]
  return(pyramid.x)
}
