#' DoORplot_tuningCurve
#' 
#' plot a receptor or odorant tuning curve
#'
#' @param receptor character; a receptor name (one of ORS$OR)
#' @param odorant character; an odorant name (InChIKey)
#' @param response.vector numerical vector; a vector with responses, if empty this is taken from responseMatrix 
#' @param responseMatrix DoOR response matrix; response vector will be taken from here, not needed if response.vector is given
#' @param zero InChIKey; will be set to zero, default is SFR, ignored when data is provided via response.vector
#' @param fill.receptor color code; bar color for receptor tuning curve 
#' @param fill.odorant  color code; bar color for odorant tuning curve 
#' @param odor.main the odor identifier to plot, one of colnamed(odor)
#' @param limits the numerical vector of length 2; y limits for the tuning curve
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' DoORplot_tuningCurve(odorant = odor$InChIKey[2])
#' DoORplot_tuningCurve(receptor = "Or22a")
#' 
#' range <- range(apply(responseMatrix, 2, function(x) resetSFR(x,x[zero])), na.rm = T)
#' DoORplot_tuningCurve(receptor = "Or10a", limits = range, fill.receptor = "magenta")
#' 
#' DoORplot_tuningCurve(receptor = "OrX", response.vector = c(1:100))
#' 
#' DoORplot_tuningCurve(odorant = "odor X", response.vector = rnorm(200))
#' 
DoORplot_tuningCurve <- function(receptor,
                                 odorant,
                                 response.vector,
                                 responseMatrix = default.val("response.matrix"),
                                 zero = default.val("zero"),
                                 fill.receptor = default.val("color.receptor"),
                                 fill.odorant  = default.val("color.odorant"),
                                 odor.main = "Name",
                                 limits
) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for AL map plotting, please install via install.packages('ggplot2')", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid is required for AL map plotting, please install via install.packages('grid')", call. = FALSE)
  
  if(missing(receptor) & missing (odorant))
    stop("either receptor or odorant has to be specified")
  if(!missing(receptor) & !missing (odorant))
    stop("only one of receptor or odorant must to be specified")
  
  if(missing(odorant)) {
    
    if(missing(response.vector)) {
      if (!is.null(zero)) {
        data <- na.omit(resetSFR(responseMatrix[ ,receptor], responseMatrix[zero, receptor]))
      } else {
        data <- na.omit(responseMatrix[ ,receptor])
      }
    } else {
      data <- na.omit(response.vector)
    }
    
    data <- data.frame(odorants = 1:length(data), value = data)
    data$odorants <- factor(data$odorants, levels = data$odorants[orderPyramid(data$value)])
    
    plot <- ggplot(data) + 
      geom_bar(aes(x = odorants, y = value), stat = "identity", position = "identity", width = 1, fill = fill.receptor) +
      theme_minimal() +
      theme(axis.text.x        = element_blank(),
            axis.ticks.x       = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.margin        = unit(c(1,.2,.5,-.2),"lines")
      ) +
      ggtitle(bquote(atop(.(
        paste(receptor, sep = "")), 
        atop(italic(.(paste0("kurtosis: ",round(sparse(data$value),2), "; n: ", nrow(data)))), ""))))
  } else {
    odorant <- as.character(odorant)
    odor.main <- odor[which(odor$InChIKey == odorant), odor.main]
    if(!missing(response.vector))
      odor.main <- odorant
    if(missing(response.vector)) {
      if (!is.null(zero)) 
        data <- apply(responseMatrix, 2, function(x) resetSFR(x,x[zero]))
      data <- na.omit(data[odorant,])
    } else {
      data <- response.vector
    }
    
    data <- data.frame(receptors = 1:length(data), value = data)
    data$receptors <- factor(data$receptors, levels = data$receptors[orderPyramid(data$value)])
    
    plot <- ggplot(data) + 
      geom_bar(aes(x = receptors, y = value), stat = "identity", position = "identity", width = 1, fill = fill.odorant) +
      theme_minimal() +
      theme(axis.text.x        = element_blank(),
            axis.ticks.x       = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.margin        = unit(c(1,.2,.5,-.2),"lines")
      ) +
      ggtitle(bquote(atop(.(
        paste(odor.main, sep = "")), 
        atop(italic(.(paste0("kurtosis: ",round(sparse(data$value),2), "; n: ", nrow(data)))), ""))))
    
  }
  
  if(!missing(limits))
    plot <- plot + coord_cartesian(ylim = limits)
  
  return(plot)
}