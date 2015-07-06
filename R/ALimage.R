#' ALimage
#'
#' Plot an antennal lobe map with color coded odorant responses.
#'
#' @param InChIKey InChIKey specifying the odorant to plot
#' @param responseMatrix the input data (e.g. response.matrix or response.matrix_non.normalized)
#' @param zero the odorant to set to zero (defaults to "SFR")
#' @param tag the labels to plot on top of the glomeruli (one of the following \code{DoOR.mappings} columns: "receptor", "sensillum", "ORN", "glomerulus" or "co.receptor") 
#' @param main the title, one column of \code{odor}, defaults to "Name"
#' @param scalebar whether or not to add a scalebar
#' @param DoOR.mappings the darta frame containing the mapping information
#' @param colors a vector containing 6 color values (2 for values below 0, 1 0 value and 3 steps between 0 and 1)
#' @param limits the limits for the color scale, if empty the range of the response matrix is taken (after setting ``zero`` to 0)
#' @details Normalized, color coded odor responses across receptors are mapped onto a map of the \emph{Drosophila} antennal lobe. The antennal lobe map was a kind gift from Veit Grabe. 
#' @seealso \link{findRespNorm}, \pkg{\link{ggplot2}, \link{grid}}
#' @references Grabe, V., Strutz, A., Baschwitz, A., Hansson, B.S., Sachse, S., 2014. A digital in vivo 3D atlas of the antennal lobe of Drosophila melanogaster. J. Comp. Neurol. n/a–n/a. doi:10.1002/cne.23697
#' @author Daniel Münch \email{daniel.muench@@uni-konstanz.de}
#' @return a ggplot2 object
#'
#' @examples
#' ALimage("MLFHJEHSLIIPHL-UHFFFAOYSA-N", scalebar = F)
#' ALimage("MLFHJEHSLIIPHL-UHFFFAOYSA-N", tag = "Ors", color = c("magenta", "pink", "white", "yellow", "orange", "red"))
#' 
#' ALimage(getKey("123-92-2"), scalebar = F) + 
#' theme(legend.position  = "bottom", 
#'       panel.background = element_rect(fill = "grey90", color = NA)) + 
#' ggtitle("responses elicited by isopentyl acetate")
#' 
#' \dontrun{
#' p <- ALimage(getKey("123-92-2"))
#' ggsave("AL.response.pdf", p, width = 6, height = 2, scale = 2)
#' }
ALimage <- function(InChIKey,
                    responseMatrix = default.val("response.matrix"),
                    zero = default.val("zero"),
                    tag =  default.val("tag.ALimage"), 
                    main = "Name",
                    scalebar = default.val("scalebar"),
                    DoOR.mappings = default.val("DoOR.mappings"),
                    AL.map = default.val("AL.map"),
                    colors = c("#0570b0","#74a9cf","#ffffff","#fdcc8a","#fc8d59","#d7301f"),
                    legend = T,
                    limits) {

  if (!require(ggplot2))
    stop("Please install lattice: install.packages('ggplot2')")
  require(grid)
  
  if(missing(limits)) {
    response.matrix.SFRreset <- apply(responseMatrix, 2, function(x) resetSFR(x,x[zero]))
    limits <- range(response.matrix.SFRreset, na.rm=T)
  }
  
  response.data <- findRespNorm(InChIKey, zero = zero, responseMatrix = responseMatrix) 
  
  plotdata <- AL.map[["glomeruli"]]
  plotdata$receptor <- DoOR.mappings$receptor[match(plotdata$glomerulus, DoOR.mappings$code)] # match mapped glomeruli and extract receptor names
  plotdata$response <- response.data$Response[match(plotdata$receptor, response.data$ORs)] # pick responses from response.data
  
  labels <- AL.map$labels
  labels <- cbind(labels,DoOR.mappings[match(labels$glomerulus, DoOR.mappings$code),c("receptor", "sensillum", "ORN", "co.receptor", "Ors")])
  
  
  main <- odor[match(InChIKey, odor$InChIKey), main]

  
  
  p <- ggplot(data = plotdata) + 
    geom_polygon(data = AL.map$background, aes(x = x, y = y, group = group), fill = "grey80", color = "grey80") +
    geom_polygon(data = AL.map$bg.cutout,  aes(x = x, y = y, group = group), fill = "white", color = "white") +
    geom_polygon(data = AL.map$unmapped_not.olf, aes(x = x, y = y, group = glomerulus), fill = "grey45", color = "grey75") +
    geom_polygon(aes(x = x, y = y, fill=response, group = glomerulus), color = "grey75") + 
    scale_fill_gradientn(na.value="grey65", colours=colors,space="rgb", values=DoORnorm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2])),limits=limits) +
    annotate("text", x = labels$x, y = labels$y, label = labels[,tag], color = "#000000", size = 3, alpha = .5) +#, hjust=0) +
    coord_fixed() + 
    theme_minimal() +
    ggtitle(main) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()#,
          #panel.background = element_rect(fill = "#eeeeee", color = "#eeeeee"),
          #legend.key.size = unit(.8, "line"),
          #legend.key.width = unit(.3, "line")
    )
  
  if(scalebar == F) 
    p <- p + theme(legend.position = "none")

  if(legend == T) {
  x1 <- 118; x2 <- x1 + 34; x3 <- x2 + 19.5; y <- -20; lsize <- 10
  p <- p + 
    annotate("rect", 
             xmin = c(x1, x2, x3) , 
             xmax = c(x1+lsize, x2+lsize, x3+lsize), 
             ymin = y, ymax = y+lsize, 
             fill = c("grey45", "grey65", "grey80")) + 
    annotate("text", 
             x = c(x1+lsize+1.5, x2+lsize+1.5, x3+lsize+1.5) , 
             y = y + lsize / 2 , 
             label = c("unmapped", "NA", "background"), hjust = 0, size = 3.5)
  }
  
  return(p)
}
