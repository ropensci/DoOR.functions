#' identifySensillum
#'
#' correlates the result from a SSR recording of several odorants against all
#' DoOR response profiles
#'
#' @param recording data frame, a data frame with the following columns
#'   "odorants" containing InChIKeys of the tested odorrant, and one column
#'   called "unit1" etc. for each unit, containing responses (or estimates)
#'   scaled between 0 and 1 (see examples)
#' @param response_matrix DoOR response matrix, the data to compair against
#' @param odor_data data frame, contains the odorant information.
#' @param DoOR_mappings the data frame containing the mapping information
#' @param tag character, the chemical identifier to use in plots, one of
#'   \code{colnames(odor)}
#' @param min.cor numeric, a minimum correlation value, the function will check
#'   wether there is a higher correlation for all units within a single
#'   sensillum
#' @param nshow numeric, the number of plots to nshow, plot e.g. only the top 10
#'   matches
#' @param plot logical, if TRUE returns the plot, else returns the data frame with the
#'   correlations/distances
#' @param method character, the method for similarity calculations: correlation
#'   ("cor") or Euclidean distances ("dist")
#' @param sub character, if you know the class of sensillum you were recording
#'   from you can restrict the search to this subset here ("ab", "ac", "at",
#'   "pb", "sac")
#' @param use character, the "use" option from the \code{cor} function, "all"
#'   returns NA when pairs are incomplete, "na.or.complete" only uses complete
#'   observations to calculate correlations; see \code{\link{cor}} for details
#' @param base_size numeric, the base font size of the ggplot plots
#'
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#'
#' @return either a plot (gtable) with responses sorted by highest correlations
#'   or lowest distances, or a data frame containing all calculated
#'   correlations or Euclidean distances
#' @export
#' @import DoOR.data
#' @importFrom stats cor na.omit dist cor.test
#'
#' @examples
#' library(DoOR.data)
#' recording <- data.frame(
#'    odorants = c(transID(c("BEDN", "ETAS"), "Code"),
#'    transID("carbon dioxide", "Name")),
#'    unit1 = c(.9,.1,.1),
#'    unit2 = c(0, .1, 1)
#' )
#'
#' identifySensillum(recording)
#' identifySensillum(recording, method = "dist", nshow = 5)
#'
identifySensillum <- function(recording,
                              response_matrix = default.val("response.matrix"),
                              odor_data       = default.val("odor"),
                              DoOR_mappings   = default.val("DoOR_mappings"),
                              tag     = "Name",
                              min.cor = .9,
                              nshow   = 10,
                              method  = "cor",
                              sub,
                              base_size = 12,
                              plot = TRUE,
                              use = "na.or.complete") {

  if(plot == TRUE) {
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("ggplot2 is required for plotting, please install via install.packages('ggplot2')", call. = FALSE)
    if (!requireNamespace("grid", quietly = TRUE))
      stop("grid is required for plotting, please install via install.packages('grid')", call. = FALSE)
    if (!requireNamespace("gridExtra", quietly = TRUE))
      stop("gridExtra is required for plotting, please install via install.packages('gridExtra')", call. = FALSE)
  }

  recording$odorants  <- as.character(recording$odorants)

  data <- response_matrix[recording$odorants,]
  if(is.na(nshow))
    nshow <- length(data)

  if(!missing(sub)) {
    subset <- as.character(DoOR_mappings$receptor[grep(paste0("^", sub, collapse = "|"), DoOR_mappings$sensillum)])
    which(colnames(data) %in% subset)
    data <- data[,which(colnames(data) %in% subset)]
  }

  if (method == "cor") {
    # calulate correlations
    units  <- colnames(recording)[-1]
    result <- data.frame(receptor = colnames(data))
    result$sensillum <- DoOR_mappings$sensillum[match(result$receptor, DoOR_mappings$receptor)]
    result$OSN <- DoOR_mappings$OSN[match(result$receptor, DoOR_mappings$receptor)]
    for(i in 1:length(units)) {
      corx <- apply(data, 2, function(x) cor(x, recording[,units[i]], use = use) )
      corx <- data.frame(receptor = names(corx), value = corx)
      colnames(corx)[2] <- units[i]
      result <- merge(result, corx)
    }

    # print results

    cor.tmp <- droplevels(subset(result, sensillum != ""))
    for(i in levels(cor.tmp$sensillum)) {
      tmp <- subset(cor.tmp, sensillum == i)
      if(all(apply(as.data.frame(tmp[,-c(1:3)]), 2 , function(x) any(na.omit(x) > min.cor)))) {
        message(paste("found correlations above ", min.cor, " for all ",length(units), " units in: ", i, sep=""))
      }
    }
  }

  if (method == "cor.test") {
    # calulate correlations
    units  <- colnames(recording)[-1]
    result <- data.frame(receptor = colnames(data))
    result$sensillum <- DoOR_mappings$sensillum[match(result$receptor, DoOR_mappings$receptor)]
    result$OSN <- DoOR_mappings$OSN[match(result$receptor, DoOR_mappings$receptor)]
    for(i in 1:length(units)) {
      corx <- c()
      px   <- c()
      for(j in colnames(data)) {
        tmp <- try(cor.test(data[,j], recording[,units[i]]), silent = TRUE)
        if(class(tmp) == "try-error") {
          corx <- c(corx, NA)
          px   <- c(px,  NA)
        } else {
          corx <- c(corx, tmp$estimate)
          px   <- c(px,  tmp$p.value)
        }
      }
      corx <- data.frame(receptor = colnames(data), value = corx, p.value = px)
      colnames(corx)[2] <- units[i]
      colnames(corx)[3] <- paste0(units[i],".pval")
      result <- merge(result, corx)
    }

    # print results

    cor.tmp <- droplevels(subset(result, sensillum != ""))
    for(i in levels(cor.tmp$sensillum)) {
      tmp <- subset(cor.tmp, sensillum == i)
      if(all(apply(as.data.frame(tmp[,-c(1:3,5)]), 2 , function(x) any(na.omit(x) > min.cor)))) {
        message(paste("found correlations above ", min.cor, " for all ",length(units), " units in: ", i, sep=""))
      }
    }
  }

  if (method == "dist") {
    # calc distances
    units  <- colnames(recording)[-1]
    result <- rbind(t(recording[,-1,drop = FALSE]), t(data))
    result <- as.matrix(dist(result))
    result <- result[(length(units)+1):nrow(result), 1:length(units),drop=FALSE]
    result <- data.frame(receptor  = rownames(result),
                         sensillum = DoOR_mappings$sensillum[match(rownames(result), DoOR_mappings$receptor)],
                         OSN = DoOR_mappings$OSN[match(rownames(result), DoOR_mappings$receptor)],
                         result)
  }


  # plots
  # change odor identifier for plotting
  if(tag != "InChIKey") {
    recording$odorants <- odor_data[match(recording$odorants, odor_data$InChIKey), tag]
    rownames(data)     <- odor_data[match(rownames(data), odor_data$InChIKey), tag]
  }

  if(plot == TRUE) {
    # plot data

    data.melt <- DoORmelt(data)
    plots <- list()
    for(i in 1:length(units)) {
      if (method %in% c("cor"))
        cor.tmp <- result[order(result[,units[i]], decreasing = TRUE),][1:nshow, c("receptor",units[i])]
      if (method %in% c("cor.test")) {
        cor.tmp <- result[order(result[,units[i]], decreasing = TRUE),][1:nshow, c("receptor",units[i],paste0(units[i],".pval"))]
        colnames(cor.tmp)[3] <- "pval"
      }
      if (method == "dist")
        cor.tmp <- result[order(result[,units[i]], decreasing = FALSE),][1:nshow, c("receptor",units[i])]

      colnames(cor.tmp)[2] <- "cor"
      cor.tmp$OSN    <- DoOR_mappings$OSN[match(cor.tmp$receptor, DoOR_mappings$receptor)]
      if(method == "cor.test"){
        cor.tmp$label  <- paste0(cor.tmp$OSN, " (",cor.tmp$receptor,")", "\n",
                                "cor: ", round(cor.tmp$cor, 3),
                                "; p: ", format(cor.tmp$pval, scientific = TRUE, digits = 2))
      } else {
        cor.tmp$label  <- paste(cor.tmp$OSN, " (",cor.tmp$receptor,")", "\n", method, ": ", round(cor.tmp$cor, 3), sep = "")
      }

      data.tmp       <- droplevels(subset(data.melt, dataset %in% cor.tmp$receptor))
      data.tmp$label <- cor.tmp$label[match(data.tmp$dataset, cor.tmp$receptor)]
      data.tmp$label <- factor(data.tmp$label, levels = cor.tmp$label)



      p <- ggplot2::ggplot(data.tmp, ggplot2::aes(x = odorant, y = value, fill = odorant, color = odorant)) +
        ggplot2::geom_bar(position = "identity", stat = "identity", alpha = .6) +
        ggplot2::facet_wrap(~ label, nrow = 1) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(panel.border  = ggplot2::element_rect(fill = NA, color = "grey"),
                       axis.ticks.x  = ggplot2::element_blank(),
                       axis.text.x   = ggplot2::element_blank(),
                       axis.title.x  = ggplot2::element_blank())
      assign(paste("p",i,sep="."),p)
    }

    # plot recording

    for(i in 1:length(units)) {
      rec.tmp <- data.frame(odorant = recording$odorants, value = recording[,units[i]], unit = paste0(units[i], "\n"))

      r <- ggplot2::ggplot(rec.tmp, ggplot2::aes(x = odorant, y = value, fill = odorant, color = odorant)) +
        ggplot2::geom_bar(position = "identity", stat = "identity", alpha=.6) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(panel.border  = ggplot2::element_rect(fill = NA, color = "grey"),
                       axis.ticks.x  = ggplot2::element_blank(),
                       axis.text.x   = ggplot2::element_blank(),
                       axis.title.x  = ggplot2::element_blank()) +
        #ggplot2::ggtitle(paste("unit", i, sep="")) +
        ggplot2::facet_wrap(~ unit) +
        ggplot2::geom_text(ggplot2::aes(y = .01, label = odorant), angle = 90, hjust = 0, vjust = .5, size = .3 * base_size, color = "black")
      assign(paste("r",i,sep="."),r)
    }

    #build grobs
    for(i in 1:length(units)) {
      plots[[i]] <- gridExtra::arrangeGrob(grobs = list(
        get(paste("r",i,sep=".")) + ggplot2::theme(legend.position = "none"),
        get(paste("p",i,sep=".")) + ggplot2::theme(legend.position = "none")
      ),
      left = units[i],
      nrow = 1, widths = c(.2,.8))
    }

    if(method == "dist")
      plots[["top"]] <- paste(nshow, "lowest euclidean distances")
    if(method == "cor")
      plots[["top"]] <- paste(nshow, "highest correlations")

    plots[["ncol"]] <- 1

    p <- do.call(gridExtra::arrangeGrob, plots)
    grid::grid.newpage()
    grid::grid.draw(p)

  } else {
    return(result)
  }
}
