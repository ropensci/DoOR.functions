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
                                    colors = default.val("colors"),
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
  
  
  
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(y = odorant, x = dataset)) + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0, vjust = .5))
  
  if(limits[1] < 0) {
    plot <- plot + 
      ggplot2::geom_tile(ggplot2::aes(fill = value)) +
      ggplot2::scale_fill_gradientn(colours  = colors, space="rgb", 
                                    values   = DoORnorm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2])),
                                    limits   = limits)
  } else {
    plot <- plot + ggplot2::geom_point(ggplot2::aes(size = value), alpha = .6) 
  }
  
  return(plot)
}
