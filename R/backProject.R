#' project the model response values back to tested values
#' 
#' project the model response values back to tested values
#' 
#' backProject is used to project the model response values back
#' to tested values.  The process of back projection is following serval steps:
#' \itemize{ \item 1. normalize two data set in values from 0 to 1; \item 2.
#' plot "bp.data" against "cons.data"; \item 3. choose the best fitting model
#' with the lowest "MD" value; \item 4. project the consensus values from "x"
#' onto fitted line, those projected Y coordinates are normalized back
#' projected values; \item 5. estimate the parameters ("intercept" and "slope")
#' between unnormalized data and normalized data using linear regression; \item
#' 6. rescale all projected Y coordinates with the parameters.  }
#' 
#' @param cons.data a data frame; containing model response values.
#' @param bp.data a data frame; containing the data will be back projected.
#' @param tag.odor a character string; indicating which row will be used for
#' matching two data.
#' @param tag.cons.data a character string; specifying which column is model
#' responses in "con.data".
#' @param tag.bp.data a character string; specifying which column in "bp.data"
#' is the data that to be back projected.
#' @return Output of backProject is a list containing a numeric
#' vector ($rescale) and a data frame ($output).  "rescale" is used to rescale
#' the normalized data to original scale. "output" contains the whole
#' "bp.data", and consensus value from "cons.data", "projected.Y" (normalized
#' back projected data) and rescaled back projected data "bp.data".
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @export
#' 
backProject <-
  function(cons.data,bp.data,tag.odor,tag.cons.data,tag.bp.data) {
    
    con.data <- as.data.frame(cons.data)
    bp.data  <- as.data.frame(bp.data)
    
    
    # match rows of the study and the model response
    
    match_two_data <- match(con.data[,tag.odor], bp.data[,tag.odor])
    
    
    # normalize the two dataframes to the range [0, 1] and bind them together
    # study             : y-axis 
    # model response    : x-axis.
    
    xy <- cbind(DoORnorm(cons.data[,tag.cons.data]), DoORnorm(bp.data[match_two_data,tag.bp.data]))
    
    
    # plot the two dataframes against each other, find the best fitting function and project
    # the points onto the functional line.
    
    calM		   <- calModel(x=xy[,1],y=xy[,2])
    projected 	   <- projectPoints(x=xy[,1],y=xy[,2],best.model=calM, plot=TRUE,title=TRUE, xlab=tag.cons.data, ylab=tag.bp.data)
    
    projected_back     <- rbind(projected$Double.Observations[,c('ID','Y')],projected$Single.Observation[,c('ID','Y')])
    #projected_back normalised:
    projected_back_NDR <- rbind(projected$Double.Observations[,c('ID','NDR')],projected$Single.Observation[,c('ID','NDR')])
    
    
    match_Double.Observations_ID <- projected$Double.Observations[, c('ID')] 		# find overlapping odors
    normalized_bp_DO 	     <- projected$Double.Observations[, c('y')]			# Y-coordinates of normalized data (Double.Observations)
    unnormalized_bp_DO 	     <- bp.data[match_Double.Observations_ID, tag.bp.data]	# Y-coordinates of unnormalized data (Double.Observations)
    
    
    
    # set unnormalized data (unnormalized_bp_DO) as response (dependent) variable, 
    # normalized data (normalized_bp_DO) as predictor (independent variable), 
    # estimate the Intercept and Slope
    
    g 	<- lm(unnormalized_bp_DO~normalized_bp_DO)
    parms 	<- c(g$coef[1], g$coef[2])
    
    
    
    # rescale data "projected_back" back to the original scale. 
    
    match_odor  		<- match(seq(1:dim(bp.data)[1]),projected_back[,'ID'])
    rescaled_projected_back <- parms[1] + parms[2] * projected_back[match_odor,'Y']	
    
    
    
    
    
    # pack into output list
    backProject <- list(rescale = c("Intercept"=as.vector(g$coef[1]),"Slope"=as.vector(g$coef[2])),
                        output  = data.frame(bp.data,
                                             consensus.value = projected_back_NDR[match_odor,"NDR"],
                                             projected.Y 	= projected_back[match_odor,"Y"],
                                             bp.data 	= rescaled_projected_back))
    return(backProject)
  }
