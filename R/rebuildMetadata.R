rebuildMetadata <- function(){

  # rebuild weight.globNorm
  tmp <- load2list()
  datasets <- unlist(sapply(tmp, function(x) colnames(x)[-c(1:default.val("num.charColumns"))]))
  datasets <- sort(unique(datasets))
  wgn <- matrix(nrow = length(ORs$OR), ncol = length(datasets))
  row.names(wgn) <- ORs$OR
  colnames(wgn)  <- datasets
  
  rr <- data.frame()
  for(i in datasets) {
    data.i <- getDataset(study = i, na.rm = T)
    rec.i  <- colnames(data.i)[-c(1:default.val("num.charColumns"))]
    # update wgn
    wgn[rec.i, i] <- 1 
    
    # update range
    range.i <- range(data.i[-c(1:default.val("num.charColumns"))], na.rm = TRUE)
    range.i <- data.frame(study = i, min = range.i[1], max = range.i[2], n_odors = nrow(data.i))
    rr <- rbind(rr, range.i)
  }
  
  wgn <- as.data.frame(wgn)
  assign("weight.globNorm", wgn, envir = .GlobalEnv)
  
  assign("response.range", rr, envir = .GlobalEnv)
  
  message("Rebuilt weight.globNorm and response.range!")
  
  # rebuild response.range
  
  
}