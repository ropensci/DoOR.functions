#' DoORplot_responseMatrix
#' 
#' plot DoOR responses as a point matrix
#'
#' @param data a subset of e.g. response.matrix
#' @param tag the chemical identfier to plot (one of colnames(odor))
#' @param colors the colors to use if negative values are supplied (range of 5 colors, 2 for negative values, 1 for 0 and 3 for positive values)
#' @param limits the limits of the scale, will be calculated if not set
#'
#' @return a dotplot if limits[1] >= 0  or a heatmap if limits[1] < 0
#' @export
#' @examples 
#' tmp <- apply(response.matrix, 2, function(x) resetSFR(x,x["SFR"]))
#' DoORplot_responseMatrix(tmp[10:50,], tag = "Name", limits = range(tmp, na.rm = T))
#' DoORplot_responseMatrix(response.matrix[10:50,], tag = "Name", limits = range(response.matrix, na.rm = T))
#'

DoORplot_responseMatrix <- function(data,
                                    tag    = default.val("tag"),
                                    colors = c("#0570b0","#74a9cf","#ffffff","#fdcc8a","#fc8d59","#d7301f"),
                                    limits) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for AL map plotting, please install via install.packages('ggplot2')", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid is required for AL map plotting, please install via install.packages('grid')", call. = FALSE)
  
  data   <- as.data.frame(data)
  if(missing(limits))
    limits <- range(data, na.rm=T)
  data   <- DoORmelt(data = data, na.rm = T)
  
  if(tag != "InChIKey") 
    data$odorant <- odor[match(data$odorant, odor$InChIKey),tag]
  
  
  
  
  plot <- ggplot(data, aes(y = odorant, x = dataset)) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = .5))
  
  if(limits[1] < 0) {
    plot <- plot + 
      geom_tile(aes(fill = value)) +
      scale_fill_gradientn(colours  = colors,space="rgb", 
                           values   = DoOR.functions:::DoORnorm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2])),
                           limits   = limits)
  } else {
    plot <- plot + geom_point(aes(size = value), alpha = .6) 
  }
  
  return(plot)
}
