#' dplot_across_ru
#'
#' barplot of DoOR responses of a set of odorant across all responding units in DoOR
#'
#' @param odorant character vecto, one or several InChIKeys
#' @param response_matrix DoOR response matrix, a DoOR response matrix as data source
#' @param odor_data data frame, contains the odorant information.
#' @param zero character, an InChIKey of the odorant that should be set to 0
#' @param tag character, the chemical identifier to plot as odorant name (one of colnames(odor))
#' @param limits numeric of length 2, if provided the ylim will range accordingly
#' @param base_size numeric, the base font size for the ggplot2 plot
#'
#' @return a ggplot object
#' @export
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases dplot_acrossReceptors dplot_across_ru
#' @examples
#' library(DoOR.data)
#' data(odor)
#' dplot_across_ru(transID("123-92-2"), tag = "CAS")
#' dplot_across_ru(odor$InChIKey[4:10])
#'
dplot_across_ru <- function(odorant,
                                     response_matrix = door_default_values("response.matrix"),
                                     odor_data = door_default_values("odor"),
                                     zero = door_default_values("zero"),
                                     tag  = "Name",
                                     limits,
                                     base_size = 12) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plotting, please install via install.packages('ggplot2')", call. = FALSE)

  if (!is.null(zero))
    response_matrix <- as.data.frame(apply(response_matrix, 2, function(x) resetSFR(x,x[zero])))

  odorant <- as.character(odorant)

  data <- response_matrix[odorant,]
  data <- door_melt(as.data.frame(data), na.rm = TRUE)

  if(tag != "InChIKey")
    data$odorant <- odor_data[match(data$odorant, odor_data$InChIKey), tag]

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = odorant, y = value, fill = odorant, color = odorant)) +
    ggplot2::geom_bar(stat = "identity", position = "identity", alpha = .6) +
    ggplot2::facet_wrap( ~ dataset) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, color = "grey"),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())

  if(!missing(limits))
    plot <- plot + ggplot2::coord_cartesian(ylim = limits)

  return(plot)

}
