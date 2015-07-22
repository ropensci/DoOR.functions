# Combine Two Data
# 
# Combine two data sets into one byInChIKey
# 
# Data frames are merged by InChIKey. Data2 will be merged into data1 specified
# by the column name \code{by.data2} into one. The name of added column could be
# assigned with \code{assigned.name}. If \code{data2} contains some odors that
# the \code{data1} do not have, then the new odors will be added into output.
# 
# @param data1 data frame; the object to be added.
# @param data2 data frame; it will be merged to "data1".
# @param by.data2 character string; a column name in "data2", specify which 
#   column will be combined into data1.
# @param assigned.name charater string; assigning a name to combined column.
# @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
# @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
# @keywords data
# @examples
# 
# library(DoOR.data)
# library(DoOR.function)
# newdata <- data.frame(InChIKey=odor$InChIKey[1:4], 
# 	new.data=c(0.4, 0.5, 0.6, 0.7))
# data(Or22a)
# new.Or22a <- combData(data1 = Or22a, data2 = newdata, by.data2 = "new.data")
# 
combData <- function(data1, data2, by.data2, assigned.name, ident = default.val("ident")) 
{
  if(any(duplicated(data2[,ident]))) stop('There are duplicated identifiers in the new dataset, please solve this first.')
  new.odor <- which(is.na(match(data2[,ident],data1[,ident])))
  
  # if no new odor
  if (is.na(new.odor[1])) {
    output 	  <- data1
    matchOdor <- match(data2[,ident], output[,ident])
    if (missing(assigned.name)) 
      assigned.name <- by.data2
    output[matchOdor, assigned.name] <- data2[, by.data2]
  } else {
    data_NewOdor <- data2[new.odor,]
    l.d1 <- length(data1[,ident])
    l.d2 <- length(data_NewOdor[,ident])
    nrow.new <- l.d1 + l.d2
    first_NewOdor <- data1[(l.d1+1),]
    output <- data1
    
    # add new odors to the frame one by one
    for (i in seq(l.d2)) {
      next_NewOdor 	     <- first_NewOdor
      next_NewOdor[,ident] <- as.character(data_NewOdor[i,ident])
      output 		     <- rbind(output,next_NewOdor)
      rownames(output)     <- seq(l.d1+i)
    }
    
    if (missing(assigned.name)) 
      assigned.name <- by.data2
    
    # add response values of new odors to the output one by one
    for (i in seq(l.d2)) {
      output[l.d1+i,assigned.name] <- data_NewOdor[i,by.data2]
    }
    
    matchOdor <- match(data2[,ident],output[,ident])
    output[matchOdor, assigned.name] <- data2[, by.data2]
  }
  
  
  return(output)
}
