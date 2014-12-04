DoORColmap <-
function(x, col.extrem = default.val("col.extrem"), color.scale = FALSE) 

# part of the DoOR package: (c) 2009 C. Giovanni Galizia, Daniel Muench, Martin Strauch, Anja Nissler, Shouwen Ma
# Neurobiology, University of Konstanz, Germany

# DoORColmap.R:
############

# DoORColmap is used for assigning a color code for for negative and positive value which is in range of [0, 1]

# paramters:
#############

#  x 		: numeric vector; consensus response data, which is in range of [0, 1].
#  col.extrem 	: character vector;  The default is c("blue", "red"), i.e. the color scale ranges from blue to red 
#  color.scale 	: logical; the default is FALSE, indicating that the result returns a color code for given value. If TRUE, the result returns the whole spectrum (255) color codes.

# output:
#############

# character vector; a color code vector for the given consensus response data.

{
	# define color ramps for the lower part (default : from "blue" to "white")
	ramp1 <- colorRamp(c(col.extrem[1], "white"))
	RGB1  <- rgb(ramp1(seq(0, 1, length = 85)), max = 255)

	# define color ramps for the upper part (default : from "white" to "red")
	ramp2 <- colorRamp(c("white", col.extrem[2]))
	RGB2  <- rgb(ramp2(seq(0, 1, length = 170)), max = 255)
	cols  <- c(RGB1, RGB2)

	subfun <- function(input) {
		if (is.na(input)) { y <- NA }
        	else
        	{
        	    if (input <= 0) 	{ y <- round((input)/(1/85)) + 85 } 
        	    else 		{ y <- round((input)/(1/(255 - 85))) + 85 }
		}
	}
	if (color.scale == TRUE) { return(cols) }
	else { mycolset <- cols[sapply(x, subfun)] }
        return(mycolset)
}
