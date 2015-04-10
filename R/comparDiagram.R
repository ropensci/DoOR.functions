#' Compare the results of two studies
#' 
#' Orderdered bar plots for two studies, allowing for an easy comparison of the
#' two studies' results.
#' 
#' 
#' @param x,y data frames; data x and y could be two different data frame or a
#' same data frame.
#' @param by.x character string; specifying a column in x.
#' @param by.y character string; specifying a column in y.
#' @param lim.y numeric vector; x-axis limit of data y.
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @keywords iplot
#' @examples
#' 
#' library(DoOR.data)
#' data(Or22a)
#' data(response.range)
#' comparDiagram(x=Or22a, y=Or22a, by.x="Hallem.2006.EN", by.y="Pelz.2006.AntEC50", lim.y=c(-0.5,7))
#' 
comparDiagram <-
function(x, y, by.x, by.y, lim.y) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany


# comparDiagram.R :
####################

# plots a diagram for comparing the results of two studies.



# parameters:
#############


#  x	 : data frame; odorant response from study 1.
#  y	 : data frame; odorant response from study 2.
#  by.x  : character string; specifying a column in x (study 1).
#  by.y  : character string; specifying a column in y (study 2).
#  lim.y : numeric vector; x-axis limit of data y.

# NOTE: data x and y could be two different data frame or a same data frame. The two compared data will be selected by specifying the column names ("by.x" and "by.y").

{
	title_x <- paste("data_X:",by.x)
	title_y <- paste("data_Y:",by.y)
	colnames(x)[which(colnames(x)==by.x)] <- title_x
	colnames(y)[which(colnames(y)==by.y)] <- title_y
	comb_xy   <- combData(data1=x,data2=y,by.data2=title_y,assigned.name=title_y)
	comb_xy   <- na.omit(comb_xy[,c("CAS", title_x,title_y)])
	sorted_xy <- comb_xy[order(comb_xy[,title_x]),]

	listed_xy <- list(CAS=sorted_xy[,"CAS"],a=sorted_xy[,title_x],b=sorted_xy[,title_y])

	layout(matrix(c(0,0,0,0,0,
			0,1,2,2,0,
			0,0,0,0,0), 
		nc = 5, byrow = TRUE),
		widths = c(lcm(2), 1, lcm(2), 0.79, lcm(2)),
		heights = c(lcm(2), 1, lcm(2)))

	ran_x 	<- range(listed_xy$a)
	ran_y 	<- range(listed_xy$b)
	ran_xy  <- range(c(ran_x,ran_y))
	lim	<- ceiling(seq(0,ran_xy[2],by=max(ran_xy)/3)) 	# define the axis labels
	op 	<- par(las=2,cex.lab=0.01,cex.axis=0.7)

	barplot(-listed_xy$a,horiz=TRUE,axes=FALSE,col="lightblue",space=0.5)
	axis(1,labels=rev(c(lim)),at=-rev(c(lim)),las=1)
	title(title_x,cex.main = 1)
	names(listed_xy$b) <- listed_xy$CAS
	op <- par(las=2,cex.lab=0.01,cex.axis=0.7)
	if (missing(lim.y)) 
	{
		lim.y <- ran_xy
		barplot(listed_xy$b,horiz=TRUE,xlim=lim.y,col="pink",space=0.5,axes=FALSE)
		axis(1,labels=c(lim),at=c(lim),las=1)
	}
	else 
	{
		barplot(listed_xy$b,horiz=TRUE,xlim=lim.y,col="pink",space=0.5,axes=FALSE)
		axis(1, las=1)
	}

	title(title_y,cex.main = 1)
}
