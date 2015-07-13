#' show the odorant responses across receptors with dot plot
#' 
#' show the odorant responses across receptors with dot plot.
#' 
#' This function is used to visualize odorant responses across receptors. The
#' given data is odors-by-receptors consensus response matrix. There are two
#' types of dot plot. In "black and white" ("BW") dotplot, a blank represents
#' no available data; the size of dot represents the intensity of odorant
#' response; filled circle and open circle response positive and negative
#' values, respectively. In "color ramp" ("CR") dotplot, the size of dot is
#' same, only difference is their colors. A blank represents no available data;
#' red circle and blue circle response positive and negative values,
#' respectively. The intensity of odorant response is indicated by color ramp.
#' The darker the color is, the more intensitve a odorant response is.
#' 
#' @param data data matrix of response values; a odors-by-receptors matrix.
#' @param type character string; indicating which dot type is used for the
#' plot. Options are "BW" and "CR" indicating "black and white" dotplot and
#' "color ramp", respectively.
#' @param dot.col character; giving the color of displayed dot in plot. The
#' default is black.
#' @param dot.size numeric; giving the size of displayed dot in plot is
#' magnified relative to the default setting. The default is 3.
#' @param col.extrem character vector; coding the color of each end. The
#' default is c("blue", "red").
#' @param cex.labels numeric; giving the size of labels is magnified relative
#' to the default setting. The default is 1.
#' @param \dots further graphical parameters (see\code{\link{par}}).
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords iplot
#' @export
#' @examples
#' 
#' library(DoOR.data)
#' data(response.matrix)
#' responseMatrix<-apply(response.matrix,2,function(x) resetSFR(x,sfr=x[1]))
#' ORdotplot(responseMatrix[c(150:240),],cex.labels=0.7,type='BW',dot.size=1.5)
#' 
ORdotplot <-
function(data,type,dot.col="black",
			dot.size=default.val("dot.size"),
			col.extrem=default.val("col.extrem"),
			cex.labels=default.val("cex.labels"),
			...) 


# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany



# ORdotplot.R :
###############

# visualises odorant responses across receptors; small dots for low responses, large dots for high responses.


# parameters:
##############


#  data 	: data matrix of response values; an odors-by-receptors matrix.
#  type 	: character string; indicating which dot type is used for the plot. Options are "BW" and "CR" indicating "black and white" dotplot and "color ramp",  respectively.
# dot.col 	: color of dot
#  dot.size 	: numeric; magnification factor for the dots. The default is 3.
#  col.extrem 	: character vector;  The default is c("blue", "red"), i.e. the color scale ranges from blue to red 
#  cex.labels 	: numeric; label size; the default is 1. 
#  ... 		: further graphical parameters "par".  

{
	
	## main program starts here

	dim_data    <- dim(data)
	x 	    <- rep(1:dim_data[2], each=dim_data[1])
	y 	    <- rep(1:dim_data[1], time=dim_data[2])
	data_vector <- c(as.matrix(data))

	# black and white dots, the symbol sizes vary with response values
	if (type=="BW")
	{
		# if there is NA in data_vector, process
		if (any(data_vector < 0, na.rm=TRUE)) 
		{
			gpch <- rep(16,length=length(data_vector))
			gpch[which(data_vector<0)]=1
		}
	# if no, the symbol is defined as circle with outline.
	else { gpch <- 21 }

	plot(x,y,xlim=c(0,(dim_data[2]+1)),ylim=c(0,(dim_data[2]+1)),type="n",axes=FALSE,xlab=NA,ylab=NA,...)
	points(x,y,pch=gpch,cex=dot.size*abs(data_vector),col=dot.col,...)
	}

	# color dots, the symbol colors vary with response values
	if (type=="CR") 
	{
		mycolset <- DoORColmap(data_vector, col.extrem = col.extrem)
		# if there is NA in data_vector, process
		if (any(is.na(data_vector))) 
		{
			gpch <- rep(21,length=length(data_vector))
			gpch[which(is.na(data_vector))] <- NA
		}
		# if no, the symbol is defined as circle with outline.
		else { gpch <- 21 }

		plot(x,y,xlim=c(0,(dim_data[2]+1)),ylim=c(0,(dim_data[2]+1)),type="n",axes=FALSE,xlab=NA,ylab=NA,...)
		points(x,y,pch=gpch,cex=dot.size,bg=mycolset,...)
	}

	axis(2,1:dim_data[1],c(rownames(data)),tick=FALSE,las=1,cex.axis=cex.labels,...)
	axis(1,1:dim_data[2],c(colnames(data)),tick=FALSE,las=2,cex.axis=cex.labels,...)
}
