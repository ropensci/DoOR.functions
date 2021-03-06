#' Compare two response profiles
#'
#' Orderdered bar plots for two studies, allowing for an easy comparison of the
#' two studies / response profiles'.
#'
#'
#' @param x the input data frame the first response profile will be taken from
#' @param y the input data frame the second response profile will be taken from
#'   (x will be taken if y is missing)
#' @param by.x character string, specifying a column in x
#' @param by.y character string, specifying a column in y
#' @param base_size numeric, the base font size for the ggplot2 plot
#' @param tag character, the chemical identifier that will be used as odorant
#'   label.
#'
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @aliases dplot_compareProfiles dplot_compare_profiles
#' @export
#' @importFrom stats na.omit
#' @examples
#' # load data
#' library(DoOR.data)
#' library(DoOR.functions)
#' data(Or22a)
#' data(door_response_range)
#' data(door_response_matrix)
#'
#' # compare the Hallem and the Pelz data set for Or22a
#' dplot_compare_profiles(x = Or22a, y = Or22a,
#'                          by.x = "Hallem.2006.EN",
#'                          by.y = "Pelz.2006.AntEC50")
#'
#' # comparedata from two different sensory neurons and add odorant labels 
#' dplot_compare_profiles(x = cbind(door_response_matrix, InChIKey =
#' rownames(door_response_matrix)), y = cbind(door_response_matrix, InChIKey =
#' rownames(door_response_matrix)), by.x = "Or22a", by.y = "Or10a")
#'
dplot_compare_profiles <- function(x, y,
                                     by.x,
                                     by.y,
                                     tag = "Name",
                                     base_size = 12) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for AL map plotting, please install via 
          install.packages('ggplot2')", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid is required for AL map plotting, please install via 
          install.packages('grid')", call. = FALSE)

  if(missing(y))
    y <- x

  if(!any(colnames(x) == "InChIKey"))
    x$InChIKey <- rownames(x)

  if(!any(colnames(y) == "InChIKey"))
    y$InChIKey <- rownames(y)

  x$odorant <- x$InChIKey
  y$odorant <- y$InChIKey
  
  title_x <- paste("data_X:", by.x)
  title_y <- paste("data_Y:", by.y)
  colnames(x)[which(colnames(x) == by.x)] <- title_x
  colnames(y)[which(colnames(y) == by.y)] <- title_y
  comb_xy   <-
    combine_data(
      data1 = x,
      data2 = y,
      by.data2 = title_y,
      assigned.name = title_y,
      ident = "odorant"
    )
  comb_xy   <- na.omit(comb_xy[, c("odorant", title_x, title_y)])
  
  if (tag != "InChIKey")
    comb_xy$odorant <-
    trans_id(comb_xy$odorant, from = "InChIKey", to = tag)
  
  sorted_xy <-
    as.character(comb_xy[rev(order(comb_xy[, title_x])), "odorant"])
  
  comb_xy <-
    door_melt(
      comb_xy,
      ident = "odorant",
      datasets = c(title_x, title_y),
      na.rm = TRUE
    )
  
  comb_xy$odorant <- factor(comb_xy$odorant, levels = sorted_xy)
  
  plot <-
    ggplot2::ggplot(comb_xy, ggplot2::aes(x = odorant, y = value, 
                                          fill = dataset)) +
    ggplot2::geom_bar(stat = "identity",
                      position = "identity",
                      width = .9) +
    ggplot2::facet_wrap( ~ dataset, scales = "free_y", nrow = 2) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = -90,
        vjust = 0.5,
        hjust = 0
      ),
      legend.position = "none"
    )
  
  return(plot)
}
