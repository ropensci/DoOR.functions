#' Receptor tuning breadth
#' 
#' Display the distribution of a receptor's responses (pyramid plot).
#' 
#' This function is used to visualise the response spectrum of a receptor.
#' Generalists have a distribution with broad tails, while specialist receptors
#' have a distinct peak that decreases sharply towards the tails.
#' 
#' @param x a numeric vector
#' @param \dots further graphical parameters, see \code{\link{par}}
#' @seealso \code{\link{barplot}}
#' @references Hallem E. A. & Carlson J. R., 2006, \emph{Coding of odors by a
#' receptor repertoire}, Cell, 125:143-160 \cr
#' \url{http://www.cell.com/content/article/abstract?uid=PIIS0092867406003631}
#' @keywords iplot
#' @export
#' @examples
#' 
#' library(DoOR.data)
#' data(Or22a)
#' x=Or22a[,"Hallem.2006.EN"]
#' x<-na.omit(x)
#' tuningBreadth(x,las=2,main="Tuning Curve:Hallem.2006.EN")
#' 
tuningBreadth <-
function(x, ...) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany



# tuningBreadth.R :
####################

# pyramidal plot showing the tuning breadth of a receptor 




# parameters:
##############


#  x	: a numerical vector (response measurements for a given receptor)
#  ... 	: further graphical parameters (as can be passed on to any R-plot)

{

	len  	  <- length(x)
	
        # divide the seqence according to data length into two parts, left part (seq1) contains singulars in ascending order;
	# right part (seq2) contains plurals in descending order.
	seq1 	  <- seq(1, len, by=2)
	seq2 	  <- rev(seq(1:len)[-c(seq1)])
	seq1_2 	  <- c(seq1, seq2)
	
        # sort the data into ascending order
	sort.data <- order(x)
	
        # rearrange the data 
	plot.data <- x[sort.data][seq1_2]
	barplot(plot.data, ...)

}
