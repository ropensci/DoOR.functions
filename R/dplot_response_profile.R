#' dplot_response_profile
#' 
#' create a barplot of a DoOR response profile
#' 
#' @param response_matrix a DoOR door_response_matrix
#' @param odor_data data frame, contains the odorant information.
#' @param receptor character, receptor name, any of
#'   colnames(door_response_matrix)
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
#' @importFrom stats na.omit
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases dplot_responseProfile dplot_response_profile
#' @examples
#' # load data
#' library(DoOR.data)
#' data(door_response_matrix)
#' 
#' # plot with default parameters
#' dplot_response_profile("Or22a", door_response_matrix)
#' 
#' # plot wit odorant names
#' dplot_response_profile("Or22a", door_response_matrix, tag = "Name")
dplot_response_profile <- function(receptor,
                 response_matrix = door_default_values("door_response_matrix"),
                 odor_data = door_default_values("odor"),
                 tag = door_default_values("tag"),
                 colored = TRUE,
                 colors = door_default_values("colors"),
                 limits,
                 zero = door_default_values("zero"),
                 scalebar = door_default_values("scalebar"),
                 base_size = 12) {
  if (zero != "")
    response_matrix <- reset_sfr(response_matrix, zero)
  
  if (missing(limits))
    limits <- range(response_matrix, na.rm = TRUE)
  
  if (limits[1] < 0) {
    values <-
      door_norm(c(limits[1], limits[1] / 2, 0, limits[2] / 3, limits[2] / 1.5, 
                  limits[2]))
  } else {
    values <- door_norm(c(0, limits[2] / 3, limits[2] / 1.5, limits[2]))
    colors <- colors[3:6]
  }
  
  response_matrix <-
    na.omit(data.frame(odorant = rownames(response_matrix), 
                       value = response_matrix[, receptor]))
  if (tag != "InChIKey")
    response_matrix$odorant <-
    odor_data[match(response_matrix$odorant, odor_data$InChIKey), tag]
  response_matrix <- response_matrix[order(response_matrix$value), ]
  response_matrix$odorant <-
    factor(response_matrix$odorant, levels = response_matrix$odorant)
  
  plot <-
    ggplot2::ggplot(response_matrix, ggplot2::aes(x = odorant, y = value)) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::coord_flip(ylim = limits) +
    ggplot2::ggtitle(receptor)
  
  if (colored == TRUE) {
    plot <-
      plot + ggplot2::geom_bar(
        ggplot2::aes(fill = value),
        stat = "identity",
        position = "identity",
        color = "grey"
      ) +
      ggplot2::scale_fill_gradientn(
        colours = colors,
        space = "rgb",
        values = values,
        limits = limits
      )
  } else {
    plot <-
      plot + ggplot2::geom_bar(stat = "identity",
                               position = "identity",
                               width = .9)
  }
  
  if (scalebar == FALSE) {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }
  
  return(plot)
  
}
