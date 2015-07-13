#' Bar Plot of Responses to Chemicals
#' 
#' Horizontal bar plot to show the response profile of an odorant receptor.
#' 
#' This function is used to visualize the response profile of an odorant
#' receptor. The data frame should contain the columns "odor class", "odor
#' name", "odor code", "CAS number" and "data". The default color range is blue
#' to red (from inhibitory to excitatory). If the data stems not from a merged
#' dataset (by using \code{\link{modelRP}}) or the recording values are not
#' normalized, only binary colors are used: blue indicates that the value is
#' smaller than 0, red indicates that the value is bigger than 0.
#' 
#' @param data data frame; odorant response data e.g. Or22a
#' @param tag character string; CAS number is the default descriptor
#' @param zero character string; where should zero be placed? e.g. zero="SFR"
#' sets zero to the spontaneous firing rate.
#' @param name.title character string; the main title of plot. Dafaults to
#' "Response Profile".
#' @param pic.height numeric; the height of the device; if missing, the height
#' will be defined by data size and \code{pic.width}.
#' @param file.name character string; file name of report; if missing, the
#' default file name is "PlotChemicals.pdf" or "PlotChemicals.jpeg".
#' @param cex.title numeric; the magnification to be used for main titles. The
#' default is 1.
#' @param x.range numeric vectors of length 2; giving the x coordinates ranges.
#' @param cex.axes numeric; the magnification to be used for axes. The default
#' is 1.
#' @param cex.tag numeric; the magnification to be used for tag. The default is
#' 1.
#' @param PDF logical; if \code{TRUE}, then the bar plot will be saved as PDF
#' file. The default is \code{FALSE}.
#' @param JPEG logical; if \code{TRUE}, then the bar plot will be saved as JPEG
#' file. The default is \code{FALSE}.
#' @param pic.width numeric; the width of the device; the default is 864.
#' @param point.size numeric; pointsize of plotted text; one point is
#' approximately one pixel, and the default is 17.
#' @param col.extrem a two characters vector; coding the colour of each end.
#' The default is c("blue", "red").
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords data
#' @export
#' @examples
#' 
#' library(DoOR.data)
#' loadRD()
#' RP.Or35a<-modelRP(Or35a)
#' # save bar plot as pdf flie
#' PlotChemicals(RP.Or35a, tag="Name", PDF=TRUE, file.name="PlotChemicals.Or35a.pdf")
#' 
#' # save bar plot as jpeg flie
#' PlotChemicals(RP.Or35a, tag="Name", zero="SFR", JPEG=TRUE)
#' 
#' # show bar plot of recording data in decreasing order
#' responses<-Or35a[,c(1:4,5)]
#' PlotChemicals(responses[order(responses[,5],decreasing=TRUE),], tag="Name")
#' 
PlotChemicals <-
function(data,
			tag = default.val("tag"),
			zero,
			name.title, pic.height, file.name,
			cex.title=1, x.range, cex.axes=1, cex.tag=1, PDF=FALSE, JPEG=FALSE, pic.width=864, point.size=17,			
			col.extrem = default.val("col.extrem") )

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# PlotChemicals.R :
####################

# Bar plot showing the response profile of an odorant receptor




# parameters:
###############


#  data 	:  data frame; odorant response data 
#  tag 		:  character string; CAS number is the default label
#  zero 	:  character string; where should zero be placed? e.g. zero="SFR" sets zero to the spontaneous firing rate.
#  name.title 	:  character string; the main title of plot. The default is "Response Profile". 
#  cex.title 	:  numeric; the magnification to be used for main titles. The default is 1. 
#  x.range 	:  numeric vectors of length 2; range of x coordinates. 
#  cex.axes 	:  numeric; the magnification to be used for axes. The default is 1. 
#  cex.tag 	:  numeric; the magnification to be used for tag. The default is 1. 
#  PDF 		:  logical; if TRUE, then the bar plot will be saved as PDF file. The default is FALSE
#  JPEG 	:  logical; if TRUE, then the bar plot will be saved as JPEG file. The default is FALSE
#  pic.width 	:  numeric; the width of the device; the default is 864.
#  pic.height 	:  numeric; the height of the device; if missing, the height will be defined by data size and pic.width
#  point.size 	:  numeric; pointsize of plotted text; one point is approximately one pixel, and the default is 17
#  file.name 	:  character string; file name of report; if missing, the default file name is "PlotChemicals.pdf" or "PlotChemicals.jpeg"
#  col.extrem 	:  a two characters vector; The default is c("blue", "red"), indicating a color range from blue to red.

{

	# plotting function for two colors
	plot.bicolor <- function(data, cex.axes, x.range, cex.tag, name.title, cex.title) 
	{
		colset 	<- ifelse(data < 0,'blue','red')
		op 	<- par(mar=c(5,11, 4, 1.7), las=2,cex.axis=cex.axes)
		barplot(data, space=0.5, horiz=TRUE, xlim = x.range, cex.names=cex.tag, col=colset, axes=FALSE)
		axis(1,las=1)
		title(name.title,cex.main=cex.title)
	}


	# plotting function for ramp colors
	plot.rampcolor <- function(data,cols,cex.axes,x.range,cex.tag,name.title,cex.title) 
	{
		layout(matrix(c(
				0,0,0,0,
				0,2,0,0,
				0,2,1,0,
				0,0,0,0),nc=4, byrow = TRUE),
				height = c(0.1, 1, 1, 0.1),
				width = c(0.5, 8,  1.6, 0.5))
		par(mar=c(4, 4, 2, 2))
		image(t(as.matrix(1:255)),col=cols,axes = FALSE)
		axis(4,labels=c("-1","0","1"),at=c(0,85/255,1),las=2,cex.axis=cex.axes)
		par(mar=c(4, 4, 2, 2))
		colset 	<- DoORColmap(data, col.extrem = col.extrem)
		op 	<- par(mar=c(5,11, 4, 1.7), las=2,cex.axis=cex.axes)
		barplot(data,space=0.5,horiz=TRUE,xlim = x.range,cex.names=cex.tag,col=colset,axes=FALSE)
		axis(1,las=1)
		title(name.title,cex.main=cex.title)
	}

        #################################################################################################
	## main programme starts here

	cols <- DoORColmap(x=null,color.scale=TRUE)

	rowCount <- 42 		# number of bars for a page
	if (is.data.frame(data))  { data_vector <- data[,5] }
	else 			  { data_vector <- data$model.response[,5] }

	if (missing(zero)) { data_vector <- data_vector }
	else 
	{
		if (is.data.frame(data)) { data_vector <- data[,5] - (data[data[,"CAS"]==zero,5]) }
		else 			 { data_vector <- data$model.response[,5] - (data$model.response[data$model.response[,"CAS"]==zero,5]) }
	}	

	if (is.data.frame(data))  { names(data_vector) <- as.character(data[,tag]) }
	else 			  { names(data_vector) <- as.character(data$model.response[,tag]) }
	

	# omit the NAs
	data_vector 	<- na.omit(data_vector)
	len_data_vector <- length(data_vector)
	rev_data_vector<-rev(data_vector)
	if (missing(x.range)) 	 { x.range 	<- c(min(data_vector,na.rm=TRUE),max(data_vector,na.rm=TRUE)) }
	if (missing(name.title)) { name.title 	<- paste("Response Profile") }
	if (missing(pic.height)) { pic.height 	<- (pic.width*len_data_vector)/42 }
	
	if (JPEG==TRUE) 
	{
		if (missing(file.name)) { jpeg("PlotChemicals.jpeg",width=pic.width, height=pic.height,pointsize=point.size) }
		else 			{ jpeg(file.name,width=pic.width, height=pic.height,pointsize=point.size) }

		if (range(data_vector)[1]<(-1) | range(data_vector)[2]>1) { plot.bicolor(data=rev_data_vector,cex.axes,x.range,cex.tag,name.title,cex.title) }
		else 							  { plot.rampcolor(data=rev_data_vector,cols,cex.axes,x.range,cex.tag,name.title,cex.title) }
		
		dev.off()
	}
	else
	{
		n_page <- 0:floor(len_data_vector/rowCount)
		if (PDF==TRUE) 
		{
			if (missing(file.name)) { pdf("PlotChemicals.pdf",width=6, height=8) }
			else 			{ pdf(file.name,width=6, height=8) }
		}
		for (i in n_page) 	
		{
			n_bar 		<- (i*rowCount+1):(i*rowCount+rowCount)
			data_vector_i 	<- rev(data_vector[n_bar])
			if (PDF==TRUE) 
			{
				if (range(data_vector)[1]<(-1) | range(data_vector)[2]>1)
				{ 
					plot.bicolor(data=data_vector_i,cex.axes,x.range,cex.tag,name.title,cex.title)
				}
				else
				{
					plot.rampcolor(data=data_vector_i,cols,cex.axes,x.range,cex.tag,name.title,cex.title)
				}
			}							
			else 
			{
				X11()
				if (range(data_vector)[1]<(-1) | range(data_vector)[2]>1)
				{
					plot.bicolor(data=data_vector_i,cex.axes,x.range,cex.tag,name.title,cex.title)
				}
				else
				{
					plot.rampcolor(data=data_vector_i,cols,cex.axes,x.range,cex.tag,name.title,cex.title)
				}
			}
		} # END for (i in n_page)
		if (PDF==TRUE) dev.off()
	}
}
