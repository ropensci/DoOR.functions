# filter_data
#
# Filter DoOR datasets before merging (remove studies with few overlapping points or SD = 0). This function is used by update_database to pre filter before performing all possible merges.
#
# @param data a DoOR data frame (e.g. Or22a)
# @param overlapValues number of minimum overlap needed between data sets
# @param charColumns number of the character columns
#
# @return a list containing the filtered data set and a data frame naming the removed data sets and the reason for removal
#
# @examples
# library(DoOR.data)
# tmp <- filter_data(Or42b)
# tmp$excluded
filter_data <- function(data, overlapValues = door_default_values("overlapValues"), charColumns = door_default_values("num.charColumns")) {

  recordColumn  <- as.numeric(c((charColumns + 1):dim(data)[2]) )
  studies       <- names(data)[recordColumn]
  receptor <- deparse(substitute(data))

  excluded <- data.frame()

  # remove studies with sd = 0 e.g. containing only 0
  study.sd <- apply(apply(as.data.frame(data[,studies]), 2, range, na.rm = TRUE), 2, sd)
  if(any(study.sd == 0)) {
    message(paste(receptor,": REMOVED ", studies[which(study.sd == 0)], " as it had a SD of 0.\n", sep = ""))
    excluded <- data.frame(study = studies[which(study.sd == 0)], reason = "SD = 0")
    data <- data[,- which(names(data) == studies[which(study.sd == 0)])]
    recordColumn  <- as.numeric( c((charColumns + 1):dim(data)[2]) )
    studies       <- names(data)[recordColumn]
  }

  # check overlap
  checkOverlap <- TRUE
  while(checkOverlap == TRUE & length(studies) > 1) {
    tmp <- data[,recordColumn]
    tmp <- !is.na(tmp)

    ol <- c()
    for(i in 1:dim(tmp)[2]) {
      a <- tmp[,i]
      b <- apply(as.data.frame(tmp[,-i]),1,function(x) any(x == TRUE))
      overlap <- length(which(a == TRUE &  b == TRUE))
      ol <- c(ol,overlap)
    }

    if(any(ol < overlapValues)) {
      remove <- which(ol < overlapValues)
      remove <- studies[remove]
      data <- data[,- which(names(data) %in% remove)]
      if(length(data) > overlapValues) {
        recordColumn  <- as.numeric(c((charColumns + 1):dim(data)[2]))
      } else {
        recordColumn <- NULL
      }
      studies       <- names(data)[recordColumn]
      message(paste(receptor, ": REMOVED ", remove, " as overlap with all other studies was smaller than ", overlapValues, "!\n", sep = ""))
      excluded <- rbind(excluded, data.frame(study = remove, reason = paste("overlap <", overlapValues)))
    } else {
      checkOverlap <- FALSE
    }
  }
  data <- list(data = data, excluded = excluded)
  return(data)
}
