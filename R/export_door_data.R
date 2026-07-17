#' export data
#' 
#' export odor response data and supported data
#' 
#' Please load ORs from data package DoOR.data by typing (\code{data(ORs)}) 
#' before use.
#' 
#' @param directory character string, output dir 
#' @param sep separator used in write.csv
#' @param ... more parameters passed to write.csv
#' @author Daniel Münch <\email{daniel.muench@@uni-giessen.de}>
#' @aliases export_door_data
#' @export
#' @importFrom utils write.csv
#' @keywords data
#' @examples
#' \dontrun{
#' # load data
#' library(DoOR.data)
#' library(DoOR.functions)
#' load_door_data()
#' 
#' # export odorant response data only
#' export_door_data(".") 	
#' }
export_door_data <-
  function(directory, sep = ";", ...) {
    all.data <- utils::data(package = "DoOR.data")
    all.data <- all.data$results[, 3]
    all.data <- all.data[-which(all.data == 'door_AL_map')]
    
    for (i in all.data) {
      
      data.i <- get(i, envir = .GlobalEnv )
      write.table(x = data.i, file = paste0(directory,'/',i,'.csv'), sep = ";", dec = ".", row.names = TRUE)
    }
    
  }
