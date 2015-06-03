#' Calculate the sparseness of a vector.
#' 
#' Sparseness can be calculated as lifetime kurtosis (LTK, Willmore and Tolhurst, 2001) or as lifetime sparseness (LTS, Bhandawat et al., 2007).
#' 
#' 
#' @param x numerical input vector
#' @param type type of sparseness measure, either 'sparse' for lifetime sparseness or 'kurtosis'.
#' @details LTS is scales between \[0,1\] while LTK is not restricted. LTS only takes positive values.
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @keywords sparseness, kurtosis
#' @references Bhandawat, V., Olsen, S.R., Gouwens, N.W., Schlief, M.L., Wilson, R.I., 2007. Sensory processing in the Drosophila antennal lobe increases reliability and separability of ensemble odor representations. Nature neuroscience 10, 1474–82. doi:10.1038/nn1976
#' @references Willmore, B., Tolhurst, D.J., 2001. Characterizing the sparseness of neural codes. Network 12, 255–270. doi:10.1080/net.12.3.255.270
#'


sparse <- function(x, method = 'ltk') {
  x <- na.omit(x)
  n <- length(x)
  
  if (type == 'ltk') {
    S <- (sum(( (x - mean(x)) / as.numeric(sd(x)) )^4)  / n) - 3
  }
  
  if (type == 'lts') {
    #if (min(x) < 0) stop('Negative values supplied, LTS can only work with positive values. Try to normalize or use type = "ltk"')
    S <-  (1 - (sum(abs(x)/n)^2 / sum(x^2/n))) # Tollhurst Equation 4
    #S <- 1/(1-1/n) * (1 - (sum(x/n)^2 / sum(x^2/n))) # Bhandawat
  }

return(S) 
}