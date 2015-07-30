#' dplot_responseProfile
#' 
#' create a barplot of a DoOR response profile
#' 
#' @param response_matrix a DoOR response.matrix
#' @param odor_data data frame, contains the odorant information.
#' @param receptor character, receptor name, any of colnames(response.matrix)
#' @param tag character, chemical identifier for annotation
#' @param colored logical, color code the bars according to the response value?
#' @param colors character vector, a vector of 5 colors (2 for values < 0, 1
#'   value for 0 and 3 values > 0)
#' @param limits numeric of length 2, the limits for the colorscale and the x
#'   axis, global range of data will be used if empty
#' @param scalebar logical, add or suppress scalebars
#' @param base_size numeric, the base font size for the ggplot2 plot
#' @param zero character, the odorant response that is set to 0, defaults to
#'   "SFR"
#'   
#' @return ggplot2 plot
#' @export
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @examples
#' library(DoOR.data)
#' data(response.matrix)
#' dplot_responseProfile("Or22a", response.matrix)
#' dplot_responseProfile("Or22a", response.matrix, tag = "Name")
dplot_responseProfile <- function(receptor,
                                     response_matrix = default.val("response.matrix"),
                                     odor_data = default.val("odor"),
                                     tag = default.val("tag"),
                                     colored = TRUE,
                                     colors = default.val("colors"),
                                     limits,
                                     zero = default.val("zero"),
                                     scalebar = default.val("scalebar"),
                                     base_size = 12
) {
  if(zero != "")
    response_matrix <- resetSFR(response_matrix, zero)
  
  if(missing(limits))
    limits <- range(response_matrix, na.rm=TRUE)
  
  if(limits[1] < 0) {
    values <- DoORnorm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2]))
  } else {
    values <- DoORnorm(c(0, limits[2]/3, limits[2]/1.5, limits[2]))
    colors <- colors[3:6]
  }
  
  response_matrix <- na.omit(data.frame(odorant = rownames(response_matrix), value = response_matrix[,receptor]))
  if(tag != "InChIKey")
    response_matrix$odorant <- odor_data[match(response_matrix$odorant, odor_data$InChIKey),tag]
  response_matrix <- response_matrix[order(response_matrix$value),]
  response_matrix$odorant <- factor(response_matrix$odorant, levels = response_matrix$odorant)
  
  plot <- ggplot2::ggplot(response_matrix, ggplot2::aes(x = odorant, y = value)) +
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
