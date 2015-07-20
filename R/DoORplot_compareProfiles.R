#' Compare two response profiles
#' 
#' Orderdered bar plots for two studies, allowing for an easy comparison of the
#' two studies / response profiles'.
#' 
#' 
#' @param by.x character string; specifying a column in x
#' @param by.y character string; specifying a column in y
#' @param x 
#' @param y 
#' @param base_size numeric; the base font size for the ggplot2 plot
#' @param tag character; the chemical identifier that will be used as odorant label.
#'
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
DoORplot_compareProfiles <- function(x, 
                                     y, 
                                     by.x, 
                                     by.y, 
                                     tag = "Name",
                                     base_size = 12) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for AL map plotting, please install via install.packages('ggplot2')", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid is required for AL map plotting, please install via install.packages('grid')", call. = FALSE)
  
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
  comb_xy   <- combData(data1 = x, data2 = y, by.data2 = title_y, assigned.name = title_y, ident = "odorant")
  comb_xy   <- na.omit(comb_xy[ ,c("odorant", title_x, title_y)])
  
  if(tag != "InChIKey")
    comb_xy$odorant <- transID(comb_xy$odorant, from = "InChIKey", to = tag)

  sorted_xy <- as.character(comb_xy[rev(order(comb_xy[ ,title_x])), "odorant"])
  
  comb_xy <- DoORmelt(comb_xy, ident = "odorant", datasets = c(title_x, title_y), na.rm = T)
  
  comb_xy$odorant <- factor(comb_xy$odorant, levels = sorted_xy)
  
  plot <- ggplot2::ggplot(comb_xy, ggplot2::aes(x = odorant, y = value, fill = dataset)) +
    ggplot2::geom_bar(stat = "identity", position = "identity", width = .9) + 
    ggplot2::facet_wrap(~ dataset, scales = "free_y", nrow = 2) + 
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust = 0), legend.position = "none")
  
  return(plot)
}
