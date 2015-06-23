#' filterData
#' 
#' filter DoOR datasets before merging (remove studies with few overlapping points or SD = 0)
#'
#' @param data a DoOR data frame (e.g. Or22a)
#' @param overlapValues number of minimum overlap needed between data sets
#' @param charColumns number of the character columns
#'
#' @return a list containing the filtered data set and a data frame naming the removed data sets and the reason for removal
#'
#' @examples 
#' tmp <- filterData(Or42b)
#' tmp$removed
filterData <- function(data, overlapValues = default.val("overlapValues"), charColumns = default.val("num.charColumns")) {	
  
  recordColumn  <- as.numeric(c((charColumns + 1):dim(data)[2]) )
  studies       <- names(data)[recordColumn]
  
  removed <- data.frame()
  
  # remove studies with sd = 0 e.g. containing only 0
  study.sd <- apply(apply(data[,studies], 2, range, na.rm = T), 2, sd)
  if(any(study.sd == 0)) {
    warning(studies[which(study.sd == 0)]," has a SD of 0 and was removed from merge.")
    removed <- data.frame(study = studies[which(study.sd == 0)], reason = "SD = 0")
    data <- data[,- which(names(data) == studies[which(study.sd == 0)])]
    recordColumn  <- as.numeric( c((charColumns + 1):dim(data)[2]) )
    studies       <- names(data)[recordColumn]
  }
  
  # check overlap
  checkOverlap <- TRUE
  while(checkOverlap == TRUE) {
    tmp <- data[,recordColumn]
    tmp <- !is.na(tmp)
    
    ol <- c()
    for(i in 1:dim(tmp)[2]) {
      a <- tmp[,i]
      b <- apply(tmp[,-i],1,function(x) any(x == TRUE))
      overlap <- length(which(a == T &  b == T))
      ol <- c(ol,overlap)
    } 
    
    if(any(ol < overlapValues)) {
      remove <- which(ol < overlapValues)
      remove <- studies[remove]
      data <- data[,- which(names(data) %in% remove)]
      recordColumn  <- as.numeric(c((charColumns + 1):dim(data)[2]))
      studies       <- names(data)[recordColumn]
      message(paste("REMOVED", remove, "as overlap with all other studies was smaller than", overlapValues, "!"))
      removed <- rbind(removed, data.frame(study = remove, reason = paste("overlap <", overlapValues)))
    } else {
      checkOverlap <- FALSE
    }
    data <- list(data = data, removed = removed)
    return(data)
  }
}