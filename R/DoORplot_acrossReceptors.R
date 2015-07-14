#' DoORplot_acrossReceptors
#'
#' barplot of DoOR responses of a set of odorants across all responding units in DoOR
#'
#' @param odorants character vecto; one or several InChIKeys
#' @param responseMatrix DoOR response matrix; a DoOR response matrix as data source
#' @param zero character; an InChIKey of the odorant that should be set to 0
#' @param tag character; the chemical identifier to plot as odorant name (one of colnames(odor))
#' @param limits numeric of length 2; if provided the ylim will range accordingly
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' DoORplot_acrossReceptors(getKey("123-92-2"), tag = "CAS")
#' DoORplot_acrossReceptors(odor$InChIKey[4:10])
#' 
DoORplot_acrossReceptors <- function(odorants,
                                     responseMatrix = default.val("response.matrix"),
                                     zero = default.val("zero"), 
                                     tag  = "Name",
                                     limits) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for AL map plotting, please install via install.packages('ggplot2')", call. = FALSE)
  
  if (!is.null(zero)) 
    responseMatrix <- as.data.frame(apply(responseMatrix, 2, function(x) resetSFR(x,x[zero])))
  
  odorants <- as.character(odorants)
  
  data <- responseMatrix[odorants,]
  data <- DoORmelt(as.data.frame(data), na.rm = T)
  
  if(tag != "InChIKey")
    data$odorant <- odor[match(data$odorant, odor$InChIKey), tag]
  
  plot <- ggplot(data, aes(x = odorant, y = value, fill = odorant, color = odorant)) + 
    geom_bar(stat = "identity", position = "identity", alpha = .6) + 
    facet_wrap( ~ dataset) + 
    theme_minimal() + 
    theme(panel.border = element_rect(fill = NA, color = "grey"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(), 
          axis.title.x = element_blank()) 
  
  if(!missing(limits)) 
    plot <- plot + coord_cartesian(ylim = limits)
  
  return(plot)
  
}