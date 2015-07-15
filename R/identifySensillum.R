#' identifySensillum
#' 
#' correlates the result from a SSR recording of several odorants against all DoOR response profiles
#'
#' @param recording data frame; a data frame with the following columns "odorants" containing InChIKeys of the tested odorrant, and one column called "unit1" etc. for each unit, containing responses (or estimates) scaled between 0 and 1 (see examples)
#' @param responseMatrix DoOR response matrix; the data to compair against
#' @param tag character; the chemical identifier to use in plots, one of \code{colnames(odor)}
#' @param min.cor numeric; a minimum correlation value, the function will check wether there is a higher correlation for all units within a single sensillum
#' @param nshow numeric; the number of plots to nshow, plot e.g. only the top 10 matches 
#' @param ret character; wether to return the "plot" or the "dataframe" with the correlations/distances
#' @param method character; the method for similarity calculations: correlation ("cor") or Euclidean distances ("dist")
#' @param sub character; if you know the class of sensillum you were recording from you can restrict the search to this subset here ("ab", "ac", "at", "pb", "sac")
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' 
#' @return either a plot with responses sorted by highest correlations or lowest distances
#' @export
#'
#' @examples
#' recording <- data.frame(odorants = c(getKey("BEDN", "Code"), getKey("ETAS", "Code"), getKey("carbon dioxide", "Name")), unit1 = c(.9,.1,.1), unit2 = c(0, .1, 1) )
#' identifySensillum(recording)
#' identifySensillum(recording, method = "dist", nshow = 5)
#' 
identifySensillum <- function(recording,
                              responseMatrix = default.val("response.matrix"),
                              tag     = "Name", 
                              min.cor = .9, 
                              nshow   = 10, 
                              ret     = "plot", 
                              method  = "cor",
                              sub) {
  
  if(ret == "plot") {  
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("ggplot2 is required for plotting, please install via install.packages('ggplot2')", call. = FALSE)
    if (!requireNamespace("grid", quietly = TRUE))
      stop("grid is required for plotting, please install via install.packages('grid')", call. = FALSE)
    if (!requireNamespace("gridExtra", quietly = TRUE))
      stop("gridExtra is required for plotting, please install via install.packages('gridExtra')", call. = FALSE)
  }
  
  recording$odorants  <- as.character(recording$odorants)
  
  data <- responseMatrix[recording$odorants,]
  if(is.na(nshow))
    nshow <- length(data)
  
  if(!missing(sub)) {
    subset <- as.character(DoOR.mappings$receptor[grep(paste0("^",sub), DoOR.mappings$sensillum)])
    which(colnames(data) %in% subset)
    data <- data[,which(colnames(data) %in% subset)]
  }
  
  if (method == "cor") {
    # calulate correlations   
    units  <- colnames(recording)[-1]
    result <- data.frame(receptor = colnames(data))
    result$sensillum <- DoOR.mappings$sensillum[match(result$receptor, DoOR.mappings$receptor)]
    result$OSN <- DoOR.mappings$OSN[match(result$receptor, DoOR.mappings$receptor)]
    for(i in 1:length(units)) {
      corx <- apply(data, 2, function(x) cor(x, recording[,units[i]]) )
      corx <- data.frame(receptor = names(corx), value = corx)
      colnames(corx)[2] <- units[i]
      result <- merge(result, corx)
    }
    
    # print results
    
    cor.tmp <- droplevels(subset(result, sensillum != ""))
    for(i in levels(cor.tmp$sensillum)) {
      tmp <- subset(cor.tmp, sensillum == i)
      if(all(apply(tmp[,-c(1:3)], 2 , function(x) any(na.omit(x) > min.cor)))) {
        message(paste("found correlations above ", min.cor, " for all ",length(units), " units in: ", i, sep=""))
      }
    }
  }
  
  if (method == "dist") {
    # calc distances
    units <- colnames(recording)[-1]
    result  <- rbind(t(recording[,-1]), t(data))
    result  <- as.matrix(dist(result))
    result  <- result[(length(units)+1):nrow(result), 1:length(units)]
    result  <- data.frame(receptor  = rownames(result), 
                          sensillum = DoOR.mappings$sensillum[match(rownames(result), DoOR.mappings$receptor)],
                          OSN = DoOR.mappings$OSN[match(rownames(result), DoOR.mappings$receptor)],
                          result)
  }
  
  
  # plots
  # change odor identifier for plotting
  if(tag != "InChIKey") {
    recording$odorants <- odor[match(recording$odorants, odor$InChIKey), tag]
    rownames(data)     <- odor[match(rownames(data), odor$InChIKey), tag]
  }
  
  # plot data
  
  data.melt <- DoORmelt(data)
  plots <- list()
  for(i in 1:length(units)) {
    if (method == "cor") 
      cor.tmp <- result[order(result[,units[i]], decreasing = T),][1:nshow, c("receptor",units[i])]
    if (method == "dist")
      cor.tmp <- result[order(result[,units[i]], decreasing = F),][1:nshow, c("receptor",units[i])]
    
    colnames(cor.tmp)[2] <- "cor"
    cor.tmp$OSN    <- DoOR.mappings$OSN[match(cor.tmp$receptor, DoOR.mappings$receptor)]
    cor.tmp$label  <- paste(cor.tmp$OSN, " (",cor.tmp$receptor,")", "\n", method, ": ", round(cor.tmp$cor, 5), sep = "")
    data.tmp       <- droplevels(subset(data.melt, dataset %in% cor.tmp$receptor))
    data.tmp$label <- cor.tmp$label[match(data.tmp$dataset, cor.tmp$receptor)]
    data.tmp$label <- factor(data.tmp$label, levels = cor.tmp$label)
    
    
    
    p <- ggplot2::ggplot(data.tmp, ggplot2::aes(x = odorant, y = value, fill = odorant, color = odorant)) +
      geom_bar(position = "identity", stat = "identity", alpha = .6) + 
      ggplot2::facet_wrap(~ label, nrow = 1) +
      ggplot2::theme_minimal() + 
      ggplot2::theme(panel.border  = ggplot2::element_rect(fill = NA, color = "grey"),
                     axis.ticks.x  = ggplot2::element_blank(),
                     axis.text.x   = ggplot2::element_blank(), 
                     axis.title.x  = ggplot2::element_blank())
    assign(paste("p",i,sep="."),p)
  }
  
  # plot recording
  
  for(i in 1:length(units)) {
    rec.tmp <- data.frame(odorant = recording$odorants, value = recording[,units[i]])
    
    r <- ggplot2::ggplot(rec.tmp, ggplot2::aes(x = odorant, y = value, fill = odorant, color = odorant)) +
      ggplot2::geom_bar(position = "identity", stat = "identity", alpha=.6) + 
      ggplot2::theme_minimal() + 
      ggplot2::theme(panel.border  = ggplot2::element_rect(fill = NA, color = "grey"),
                     axis.ticks.x  = ggplot2::element_blank(),
                     axis.text.x   = ggplot2::element_blank(), 
                     axis.title.x  = ggplot2::element_blank()) + 
      #ggplot2::ggtitle(paste("unit", i, sep="")) +
      ggplot2::geom_text(aes(y = .01, label = odorant), angle = 90, hjust = 0, vjust = .5, size = 3, color = "black")
    assign(paste("r",i,sep="."),r)
  }
  
  #build grobs
  for(i in 1:length(units)) {
    plots[[i]] <- arrangeGrob(get(paste("r",i,sep=".")) + theme(legend.position = "none"),
                              get(paste("p",i,sep=".")) + theme(legend.position = "none"), 
                              left = paste("unit",i),
                              nrow = 1, widths = c(.2,.8))
  }
  
  if(method == "dist")
    plots[["main"]] <- paste(nshow, "lowest euclidean distances")
  if(method == "cor")
    plots[["main"]] <- paste(nshow, "highest correlations")
  
  p <- do.call(arrangeGrob, plots)
  
  
  if(ret == "plot")
    return(p)
  if(ret == "dataframe")
    return(result)
  
}