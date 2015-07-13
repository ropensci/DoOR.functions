#' DoORmelt
#' 
#' prepare odor response data for plotting via ggplot2
#'
#' @param data data frame containing identifier and data columns
#' @param ident chemical identifier, defaults to "InChIKey"
#' @param datasets names of the data containing columns
#' @param na.rm should NAs be removed
#'
#' @return "long" data frame for plotting with ggplot2
#'
#' @examples
#' DoORmelt(Or22a, datasets = c("Hallem.2004.WT", "Pelz.2006.AntEC50"))
#' 
DoORmelt <- function(data, datasets, ident = default.val("ident"), na.rm = F) {
  result <- data.frame()
  for (i in 1:length(datasets)) {
    tmp <- data.frame(odorant = data[,ident], dataset = datasets[i], value = data[,datasets[i]])
    result <- rbind(result, tmp)
  }
  
  if(na.rm == T)
    result <- na.omit(result)
  
  return(result)
}