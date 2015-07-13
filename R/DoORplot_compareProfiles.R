#' Compare two response profiles
#' 
#' Orderdered bar plots for two studies, allowing for an easy comparison of the
#' two studies / response profiles'.
#' 
#' 
#' @param x,y data frames; if only x is given, both data sets are taken from x
#' @param by.x character string; specifying a column in x
#' @param by.y character string; specifying a column in y
#' @param tag name of the column that holds the chemical identifiers for merging the data (will also be used as label)
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @export
#' @examples
#' 
#' library(DoOR.data)
#' data(Or22a)
#' data(response.range)
#' DoORplot_compareProfiles(x=Or22a, y=Or22a, by.x="Hallem.2006.EN", by.y="Pelz.2006.AntEC50")
#' DoORplot_compareProfiles(x = cbind(response.matrix, InChIKey = rownames(response.matrix)), y = cbind(response.matrix, InChIKey = rownames(response.matrix)), by.x = "Or22a", by.y = "Or10a", lim.y=c(0,1))
#' 
DoORplot_compareProfiles <- function(x, y, by.x, by.y, tag = default.val(DoOR_default = "tag")) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for AL map plotting, please install via install.packages('ggplot2')", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid is required for AL map plotting, please install via install.packages('grid')", call. = FALSE)
  
  if(missing(y))
    y <- x
  
  title_x <- paste("data_X:", by.x)
  title_y <- paste("data_Y:", by.y)
  colnames(x)[which(colnames(x) == by.x)] <- title_x
  colnames(y)[which(colnames(y) == by.y)] <- title_y
  comb_xy   <- combData(data1 = x, data2 = y, by.data2 = title_y, assigned.name = title_y)
  comb_xy   <- na.omit(comb_xy[ ,c(tag, title_x, title_y)])
  
  sorted_xy <- as.character(comb_xy[rev(order(comb_xy[ ,title_x])), tag])
  
  comb_xy <- DoORmelt(comb_xy, ident = tag, datasets = c(title_x, title_y), na.rm = T)
  
  comb_xy$odorant <- factor(comb_xy$odorant, levels = sorted_xy)
  
  plot <- ggplot2::ggplot(comb_xy, ggplot2::aes(x = odorant, y = value, fill = dataset)) +
    ggplot2::geom_bar(stat = "identity", width = .9) + 
    ggplot2::facet_wrap(~ dataset, scales = "free_y", nrow = 2) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust = 0), legend.position = "none")
  
  return(plot)
}
