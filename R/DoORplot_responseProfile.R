#' DoORplot_responseProfile
#' 
#' create a barplot of a DoOR response profile
#'
#' @param responseMatrix a DoOR response.matrix
#' @param receptor character; receptor name, any of colnames(response.matrix)
#' @param tag character; chemical identifier for annotation
#' @param colored logical; color code the bars according to the response value?
#' @param colors character vector; a vector of 5 colors (2 for values < 0, 1 value for 0 and 3 values > 0)
#' @param limits numeric of length 2; the limits for the colorscale and the x axis, global range of data will be used if empty
#' @param scalebar logical; add or suppress scalebars
#' @param base_size numeric; the base font size for the ggplot2 plot
#' @param zero character; the odorant response that is set to 0, defaults to "SFR"
#'
#' @return ggplot2 plot
#' @export
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @examples 
#' DoORplot_responseProfile(response.matrix, "Or22a")
#' DoORplot_responseProfile(response.matrix, "Or22a", tag = "Name")
DoORplot_responseProfile <- function(receptor,
                                     responseMatrix = default.val("response.matrix"),
                                     tag = default.val("tag"),
                                     colored = TRUE,
                                     colors = default.val("colors"),
                                     limits,
                                     zero = default.val("zero"),
                                     scalebar = default.val("scalebar"), 
                                     base_size = 12
) {
  if(zero != "")
    responseMatrix <- resetSFR(responseMatrix, zero)
  
  if(missing(limits))
    limits <- range(responseMatrix, na.rm=TRUE)
  
  if(limits[1] < 0) {
    values <- DoORnorm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2]))
  } else {
    values <- DoORnorm(c(0, limits[2]/3, limits[2]/1.5, limits[2]))
    colors <- colors[3:6]
  }
  
  responseMatrix <- na.omit(data.frame(odorant = rownames(responseMatrix), value = responseMatrix[,receptor]))
  if(tag != "InChIKey") 
    responseMatrix$odorant <- odor[match(responseMatrix$odorant, odor$InChIKey),tag]
  responseMatrix <- responseMatrix[order(responseMatrix$value),]
  responseMatrix$odorant <- factor(responseMatrix$odorant, levels = responseMatrix$odorant)
  
  plot <- ggplot2::ggplot(responseMatrix, ggplot2::aes(x = odorant, y = value)) + 
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::coord_flip(ylim = limits) +
    ggplot2::ggtitle(receptor)
  
  if(colored == TRUE) {
    plot <- plot + ggplot2::geom_bar(ggplot2::aes(fill = value), stat = "identity", position = "identity", color = "grey") +
      ggplot2::scale_fill_gradientn(colours = colors, space = "rgb", values = values, limits = limits)
  } else {
    plot <- plot + ggplot2::geom_bar(stat = "identity", position = "identity", width = .9)
  }
  
  if(scalebar == FALSE) {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }
  
  return(plot)
  
}
