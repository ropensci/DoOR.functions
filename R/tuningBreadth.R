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
