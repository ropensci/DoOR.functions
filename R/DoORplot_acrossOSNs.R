#' DoORplot_acrossOSNs
#' 
#' plot the activation patterns of one or several odorants across OSNs
#'
#' @param odorants character vector; one or several InChIKeys
#' @param responseMatrix DOOR response matrix; contains the data to plot
#' @param zero character; InChIKey of the odorant that should be set to 0 (e.g. SFR)
#' @param tag character; the chemical identifier to use in the plot, one of \code{colnames(odor)}
#' @param plot.type interger; 1 or 2, defining the type of plot (1: facet_grid of odorants * sensillae; 2: facet_wrap across OSNs)
#' @param base_size numeric; the base font size for the ggplot2 plot
#' @param sub character vector; specify one or several classes of sensilla the plot should be restricted to. One or several of: "ab" "ac", "at", "ai", "pb", "sacI", "sacII"
#' 
#' @details DoOR response profiles will be selected and ordered according to the OSNs they are related to. Several DoOR response profiles might exist for a given OSN (e.g. one for the OSN itself and one for the OSNs misexpressed receptor protein) but only one will be shown. Which DoOR profile is mapped to which OSN is controlled via the "code.OSN" column in \code{DoORmapings}.
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' odorants <- transID(c("1-butanol", "isopentyl acetate", "carbon dioxide", "water"), "Name", "InChIKey")
#' DoORplot_acrossOSNs(odorants)
#' DoORplot_acrossOSNs(odorants, sub = c("ac", "at"))
#' DoORplot_acrossOSNs(odorants, plot.type = 2)
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
DoORplot_acrossOSNs <- function(odorants,
                                responseMatrix = default.val("response.matrix"),
                                sub,
                                zero = default.val("zero"), 
                                tag  = "Name", 
                                plot.type = 1,
                                base_size = 12) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plotting, please install via install.packages('ggplot2')", call. = FALSE)
  
  if (!is.null(zero)) 
    responseMatrix <- as.data.frame(apply(responseMatrix, 2, function(x) resetSFR(x,x[zero])))
  
  odorants <- as.character(odorants)
  
  data <- responseMatrix[odorants,]
  data <- DoORmelt(as.data.frame(data), na.rm = F)
  
  if(tag != "InChIKey")
    data$odorant <- odor[match(data$odorant, odor$InChIKey), tag]
  
  data$sensillum <- DoOR.mappings[match(data$dataset, DoOR.mappings$receptor), "sensillum"]
  data$OSN       <- DoOR.mappings[match(data$dataset, DoOR.mappings$receptor), "code.OSN"]
  data           <- subset(data, sensillum != "")
  data$OSN       <- gsub("ab|ac|at|ai|pb|sacI|sacII|[0-9]", "", data$OSN)
  data           <- subset(data, !is.na(OSN))
  data$group     <- paste(data$sensillum, data$OSN, " (", data$dataset,")", sep="")
  
  
  # resort sensillum levels (ab9 and ab10)
  w9 <- which(levels(data$sensillum) == "ab9")
  w10 <- which(levels(data$sensillum) == "ab10")
  data$sensillum <- factor(data$sensillum, levels = levels(data$sensillum)[c(1:(w10-1), (w10+1):(w9-1),w9,w10, (w9+1):length(levels(data$sensillum)))])
  
  # resort group levels (the fast, less failsave way)
  data$group <- factor(data$group)
  data$group <- factor(data$group, levels = levels(data$group)[c(3:21, 1:2, 22:length(levels(data$group)))])
  
  if(!missing(sub))
    data <- data[grep(paste0("^",sub, collapse = "|"), data$sensillum),]
  
  if (plot.type == 1) { 
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = OSN, y = value, fill = OSN, color = OSN)) + 
      ggplot2::geom_bar(stat = "identity", position = "identity", alpha = .6) + 
      ggplot2::facet_grid(odorant ~ sensillum) + 
      ggplot2::theme_minimal(base_size = base_size) + 
      ggplot2::theme(panel.border  = ggplot2::element_rect(fill = NA, color = "grey"),
                     axis.ticks.x  = ggplot2::element_blank(),
                     axis.text.x   = ggplot2::element_blank(), 
                     axis.title.x  = ggplot2::element_blank()) + 
      ggplot2::geom_text(ggplot2::aes(label = dataset), angle = -90, size = .3*base_size, y = -.02, alpha = .9, hjust = 0)
  }
  
  if (plot.type == 2) { 
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = odorant, y = value, fill = odorant, color = odorant)) + 
      ggplot2::geom_bar(stat = "identity", position = "identity", alpha = .6) + 
      ggplot2::facet_wrap(~ group) + 
      ggplot2::theme_minimal(base_size = base_size) + 
      ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, color = "grey"),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x  = ggplot2::element_blank(), 
                     axis.title.x = ggplot2::element_blank())
  }
  
  return(plot)
  
}
