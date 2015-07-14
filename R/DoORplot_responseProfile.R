#' DoORplot_responseProfile
#' 
#' create a barplot of a DoOR response profile
#'
#' @param data a DoOR response.matrix
#' @param receptor character string, receptor name, any of colnames(response.matrix)
#' @param tag chemical identifier for annotation
#' @param colored color code the bars according to the response value?
#' @param colors a vector of 5 colors (2 for values < 0, 1 value for 0 and 3 values > 0)
#' @param limits the limits for the colorscale and the x axis, global range of data will be used if empty
#' @param scalebar add or suppress scalebars
#'
#' @return ggplot2 plot
#' @export
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @examples 
#' DoORplot_responseProfile(response.matrix, "Or22a")
#' DoORplot_responseProfile(response.matrix, "Or22a", tag = "Name")
DoORplot_responseProfile <-
  function(data,
           receptor,
           tag = default.val("tag"),
           colored = T,
           colors = default.val("colors"),
           limits,
           scalebar = default.val("scalebar")
  ) {
    
    if(missing(limits) & colored == T)
      limits <- range(data, na.rm=T)
    
    data <- na.omit(data.frame(odorant = rownames(data), value = data[,receptor]))
    if(tag != "InChIKey") 
      data$odorant <- odor[match(data$odorant, odor$InChIKey),tag]
    data <- data[order(data$value),]
    data$odorant <- factor(data$odorant, levels = data$odorant)

    plot <- ggplot(data, ggplot2::aes(x = odorant, y = value)) + 
      theme_minimal() +
      coord_flip(ylim = limits) +
      ggtitle(receptor)
    
    if(colored == T) {
      plot <- plot + geom_bar(aes(fill = value), stat = "identity", position = "identity", color = "lightgrey") +
        scale_fill_gradientn(colours = colors, space = "rgb", values = DoOR.functions:::DoORnorm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2])),limits=limits)
    } else {
      plot <- plot + geom_bar(stat = "identity", position = "identity", width = .9)
    }
    
    if(scalebar == FALSE) {
      plot <- plot + theme(legend.position = "none")
    }
    
    return(plot)
    
  }
