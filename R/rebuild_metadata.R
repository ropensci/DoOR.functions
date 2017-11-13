rebuild_metadata <- function() {
  # rebuild door_global_normalization_weights
  tmp <- load2list()
  datasets <-
    unlist(lapply(tmp, function(x)
      colnames(x)[-c(1:door_default_values("num.charColumns"))]))
  datasets <- sort(unique(datasets))
  wgn <- matrix(nrow = length(ORs$OR), ncol = length(datasets))
  row.names(wgn) <- ORs$OR
  colnames(wgn)  <- datasets
  
  rr <- data.frame()
  for (i in datasets) {
    data.i <- get_dataset(study = i, na.rm = TRUE)
    rec.i  <-
      colnames(data.i)[-c(1:door_default_values("num.charColumns"))]
    # update wgn
    wgn[rec.i, i] <- 1
    
    # update range
    range.i <-
      range(data.i[-c(1:door_default_values("num.charColumns"))], na.rm = TRUE)
    range.i <-
      data.frame(
        study = i,
        min = range.i[1],
        max = range.i[2],
        n_odors = nrow(data.i)
      )
    rr <- rbind(rr, range.i)
  }
  
  wgn <- as.data.frame(wgn)
  assign("door_global_normalization_weights", wgn, envir = .GlobalEnv)
  
  assign("door_response_range", rr, envir = .GlobalEnv)
  
  message("Rebuilt door_global_normalization_weights and door_response_range!")
  
  # rebuild door_response_range
  
  
}
