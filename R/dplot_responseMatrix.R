#' dplot_responseMatrix
#'
#' plot DoOR responses as a point matrix
#'
#' @param data a subset of e.g. response.matrix
#' @param odor_data data frame, contains the odorant information.
#' @param tag the chemical identfier to plot (one of colnames(odor))
#' @param colors the colors to use if negative values are supplied (range of 5
#'   colors, 2 for negative values, 1 for 0 and 3 for positive values)
#' @param limits the limits of the scale, will be calculated if not set
#' @param bw logical, whether to plot b&w or colored
#' @param point logical, if \code{TRUE} a point matrix instead of a heatmap will
#'   be returned (the default if you supply only positive values)
#' @param base_size numeric, the base font size for the ggplot2 plot
#' @param flip logical, if TRUE the x and y axes will be flipped
#' @param fix logical, whether to fix the ratio of the tiles when plotting as a heatmap
#'
#' @return a dotplot if limits[1] >= 0  or a heatmap if limits[1] < 0
#' @export
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @examples
#' library(DoOR.data)
#' data(response.matrix)
#' tmp <- resetSFR(response.matrix, "SFR")
#' dplot_responseMatrix(tmp[10:50,], tag = "Name", limits = range(tmp, na.rm = TRUE))
#' dplot_responseMatrix(response.matrix[10:50,], tag = "Name",
#'                         limits = range(response.matrix, na.rm = TRUE))
#'
dplot_responseMatrix <- function(data,
                                 odor_data = door_default_values("odor"),
                                 tag    = door_default_values("tag"),
                                 colors = door_default_values("colors"),
                                 flip = FALSE,
                                 fix = TRUE,
                                 bw     = FALSE,
                                 point  = FALSE,
                                 limits,
                                 base_size = 12) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plotting, please install via install.packages('ggplot2')", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid is required for plotting, please install via install.packages('grid')", call. = FALSE)
  
  data   <- as.data.frame(data)
  
  # define limits and map colors of the colorscale
  if(missing(limits))
    limits <- range(data, na.rm=TRUE)
  
  if(limits[1] < 0) {
    values <- door_norm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2]))
  } else {
    values <- door_norm(c(0, limits[2]/3, limits[2]/1.5, limits[2]))
    colors <- colors[3:6]
  }
  
  data   <- door_melt(data = data, na.rm = TRUE)
  
  if(tag != "InChIKey")
    data$odorant <- odor_data[match(data$odorant, odor_data$InChIKey),tag]
  
  if(bw == TRUE & point == FALSE) {
    bw <- FALSE
    message("Plotting black&white heatmaps does not work, ignoring 'bw = TRUE' ")
  }
  
  if(bw == TRUE & point == TRUE) {
    bw <- FALSE
    message("Sorry, but we can't plot negative sized points, ignoring 'bw = FALSE'.")
  }
  
  if(missing(point) & missing(bw) & limits[1] >= 0) {
    point <- TRUE
    bw    <- TRUE
    message("Only positive values, returning b&w point plot.")
  }
  
  if(flip == TRUE) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = odorant, y = dataset)) 
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(y = odorant, x = dataset)) 
  }
  
  
  plot <- plot + 
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0, vjust = .5))
  
  if(bw == FALSE & point == FALSE)
    plot <- plot + ggplot2::scale_fill_gradientn(colours  = colors, space="rgb", values = values, limits  = limits)
  if(bw == FALSE & point == TRUE)
    plot <- plot + ggplot2::scale_color_gradientn(colours  = colors, space="rgb", values = values, limits  = limits)
  
  if(point == FALSE) {
    plot <- plot + ggplot2::geom_tile(ggplot2::aes(fill = value))
  } else {
    if(bw == FALSE) {
      if(limits[1] < 0) {
        plot <- plot + ggplot2::geom_point(ggplot2::aes(size = abs(value), color = value), alpha = .6)
      } else {
        plot <- plot + ggplot2::geom_point(ggplot2::aes(size = value, color = value), alpha = .6)
      }
    } else {
      if(limits[1] < 0) {
        stop("Sorry, but we can't plot negative sized points, please try again with 'bw = FALSE'.")
      } else {
        plot <- plot + ggplot2::geom_point(ggplot2::aes(size = value), alpha = .6)
      }
    }
  }
  
  if (fix == TRUE)
    plot <- plot + ggplot2::coord_fixed()
  
  return(plot)
}
