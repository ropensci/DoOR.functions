ALimage <- function(InChIKey,
                    responseMatrix = default.val("response.matrix"),
                    zero = default.val("zero"),
                    tag =  default.val("tag.ALimage"), 
                    main = "Name",
                    ind.panels = default.val("ind.panels"),
                    DoOR.mappings = default.val("DoOR.mappings"),
                    colors = c("#0570b0","#74a9cf","#ffffff","#fdcc8a","#fc8d59","#d7301f"), 
                    limits) {
  require(ggplot2)
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
    geom_polygon(data = AL.map$background, aes(x = x, y = y, group = group), fill = "grey65", color = "grey65") +
    geom_polygon(data = AL.map$unmapped_not.olf, aes(x = x, y = y, group = glomerulus), fill = "grey50", color = "grey75") +
    geom_polygon(data = AL.map$bg.cutout,  aes(x = x, y = y, group = group), fill = "white", color = "white") +
    geom_polygon(aes(x = x, y = y, fill=response, group = glomerulus), color = "grey75") + 
    scale_fill_gradientn(na.value="grey80", colours=colors,space="rgb", values=DoORnorm(c(limits[1], limits[1]/2, 0, limits[2]/3, limits[2]/1.5, limits[2])),limits=limits) +
    annotate("text", x = labels$x, y = labels$y, label = labels[,tag], color = "#000000", size = 3, alpha = .4) +#, hjust=0) +
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
  return(p)
}