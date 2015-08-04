#' dplot_tuningCurve
#'
#' plot a receptor or odorant tuning curve
#'
#' @param receptor character, a receptor name (one of ORS$OR)
#' @param odorant character, an odorant name (InChIKey)
#' @param response.vector numerical vector, a vector with responses, if empty
#'   this is taken from response.matrix
#' @param response_matrix DoOR response matrix, response vector will be taken
#'   from here, not needed if response.vector is given
#' @param odor_data data frame, contains the odorant information.
#' @param zero InChIKey, will be set to zero, default is SFR, ignored when data
#'   is provided via response.vector, set to "" if you don't want to subtract
#'   anything
#' @param fill.receptor color code, bar color for receptor tuning curve
#' @param fill.odorant  color code, bar color for odorant tuning curve
#' @param odor.main the odor identifier to plot, one of colnamed(odor)
#' @param base_size numeric, the base font size for the ggplot2 plot
#' @param limits the numerical vector of length 2, y limits for the tuning curve
#'
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' library(DoOR.data)
#' data(response.matrix)
#' data(odor)
#' 
#' dplot_tuningCurve(odorant = odor$InChIKey[2])
#' dplot_tuningCurve(receptor = "Or22a")
#'
#' range <- range(resetSFR(response.matrix, "SFR"), na.rm = TRUE)
#' dplot_tuningCurve(receptor = "Or10a", limits = range, fill.receptor = "magenta")
#'
#' dplot_tuningCurve(receptor = "OrX", response.vector = c(1:100))
#'
#' dplot_tuningCurve(odorant = "odor X", response.vector = rnorm(200))
#'
dplot_tuningCurve <- function(receptor,
                                 odorant,
                                 response.vector,
                                 response_matrix = default.val("response.matrix"),
                                 odor_data = default.val("odor"),
                                 zero = default.val("zero"),
                                 fill.receptor = default.val("color.receptor"),
                                 fill.odorant  = default.val("color.odorant"),
                                 odor.main = "Name",
                                 limits,
                                 base_size = 12
) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plotting, please install via install.packages('ggplot2')", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid is required for plotting, please install via install.packages('grid')", call. = FALSE)

  if(missing(receptor) & missing (odorant))
    stop("either receptor or odorant has to be specified")
  if(!missing(receptor) & !missing (odorant))
    stop("only one of receptor or odorant must to be specified")

  if(missing(odorant)) {

    if(missing(response.vector)) {
      if (!zero == "") {
        data <- na.omit(resetSFR(response_matrix[ ,receptor], response_matrix[zero, receptor]))
      } else {
        data <- na.omit(response_matrix[ ,receptor])
      }
    } else {
      data <- na.omit(response.vector)
    }

    data <- data.frame(odorants = 1:length(data), value = data)
    data$odorants <- factor(data$odorants, levels = data$odorants[orderPyramid(data$value)])

    plot <- ggplot2::ggplot(data) +
      ggplot2::geom_bar(ggplot2::aes(x = odorants, y = value), stat = "identity", position = "identity", width = 1, fill = fill.receptor) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(axis.text.x        = ggplot2::element_blank(),
                     axis.ticks.x       = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin        = grid::unit(c(1,.2,.5,-.2),"lines")
      ) +
      ggplot2::ggtitle(bquote(atop(.(
        paste(receptor, sep = "")),
        atop(italic(.(paste0("kurtosis: ",round(sparse(data$value),2), ", n: ", nrow(data)))), ""))))
  } else {
    odorant <- as.character(odorant)
    odor.main <- odor_data[which(odor_data$InChIKey == odorant), odor.main]
    if(!missing(response.vector))
      odor.main <- odorant
    if(missing(response.vector)) {
      if (!zero == "")
        data <- apply(response_matrix, 2, function(x) resetSFR(x, x[zero]))
      data <- na.omit(data[odorant,])
    } else {
      data <- response.vector
    }

    data <- data.frame(receptors = 1:length(data), value = data)
    if(nrow(data) > 1)
      data$receptors <- factor(data$receptors, levels = data$receptors[orderPyramid(data$value)])

    plot <- ggplot2::ggplot(data) +
      ggplot2::geom_bar(ggplot2::aes(x = receptors, y = value), stat = "identity", position = "identity", width = 1, fill = fill.odorant) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(axis.text.x        = ggplot2::element_blank(),
                     axis.ticks.x       = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin        = grid::unit(c(1,.2,.5,-.2),"lines")
      ) +
      ggplot2::labs(x = "responding units") +
      ggplot2::ggtitle(bquote(atop(.(
        paste(odor.main, sep = "")),
        atop(italic(.(paste0("kurtosis: ",round(sparse(data$value),2), ", n: ", nrow(data)))), ""))))

  }

  if(!missing(limits))
    plot <- plot + ggplot2::coord_cartesian(ylim = limits)

  return(plot)
}
