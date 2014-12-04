SSexpo <-
structure(function (x, a, b, c) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

{
    .expr2 <- exp(c * x)
    .value <- a + b * .expr2
    .grad <- array(0, c(length(.value), 3L), list(NULL, c("a", 
        "b", "c")))
    .grad[, "a"] <- 1
    .grad[, "b"] <- .expr2
    .grad[, "c"] <- b * (.expr2 * x)
    attr(.value, "gradient") <- .grad
    .value
}, initial = function(mCall, data, LHS)

## selfStart model estimates the parameters (a,b,c) of expontenial model.
#  y : a numeric vector indicates output of the equation. 
#  x : a numeric vector indicates input of the equation. 
#  a : a numeric parameter. 
#  b : a numeric parameter. 
#  c : a numeric parameter. 

## estimation process is following: 
#  expr:y = a + b*exp(c * x)
#  1.   initial guess: a (est_a)
#  2.	transfer to linear function: log(y - est_a) = log(b) + c * x 
#  3.	use lm() to estimate log(b) and c

  {
    # Create a sortedXyData object
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if (nrow(xy) < 4) {
        stop("too few distinct input values to fit the 'exponential' model")
    }
    est_a <- NLSstLfAsymptote(xy)
    diff  <- abs(xy$y - est_a)
    g 	  <- lm(diff ~ xy[,'x'])
    est_b <- exp(g$coef[1])
    est_c <- g$coef[2]
    value <- c(est_a, est_b, est_c)
    names(value) <- mCall[c("a", "b", "c")]
    value
  }, pnames = c("a", "b", "c"), class = "selfStart")
