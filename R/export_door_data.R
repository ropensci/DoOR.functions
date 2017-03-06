#' export data
#'
#' export odor response data and supported data
#'
#' Please load ORs from data package DoOR.data by typing (\code{data(ORs)})
#' before use.
#'
#' @param file.format character string, the format of given file, either ".txt"
#' or ".csv"
#' @param directory character string, naming a directory for writing. If
#' missing, the exported data are saved in current working directory.
#' @param odorantReceptors data frame, receptor names and expressions
#' @param response_matrix data matrix, an global unnormalized responses matrix
#' @param responseRange data frame, response ranges for each study
#' @param unglobalNorm_RM data matrix, an unnormalized responses matrix
#' @param weightGlobNorm data frame, weight matrix for global normalizazion
#' @param all.data logical, if TRUE, export odorant response data and supported
#' data "door_response_matrix", "door_response_range", "door_response_matrix_non_normalized",
#' "door_response_matrix", "door_global_normalization_weights" and "ORs".
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @aliases export_door_data exportData
#' @export
#' @importFrom utils write.csv write.table
#' @keywords data
#' @examples
#' \dontrun{
#' # load data
#' library(DoOR.data)
#' library(DoOR.functions)
#' load_door_data()
#' 
#' # export odorant response data only
#' export_door_data(".txt", all.data = FALSE) 	
#' }
export_door_data <-
  function(file.format, directory,
           odorantReceptors = door_default_values("ORs"),
           response_matrix = door_default_values("door_response_matrix"),
           responseRange = door_default_values("door_response_range"),
           unglobalNorm_RM = door_default_values("door_response_matrix_non_normalized"),
           weightGlobNorm = door_default_values("door_global_normalization_weights"),
           all.data = TRUE ) {

    if (missing(directory)) { directory <- dir() }

    for (i in as.vector(odorantReceptors[,"OR"])) {
      or.name <- i
      da 	<- get(or.name)
      if (file.format == ".txt") { write.table(da, paste(or.name,file.format,sep="") ) }
      if (file.format == ".csv") { write.csv(da, paste(or.name,file.format,sep="") ) }
    }

    if (all.data == TRUE) {

      if (file.format == ".txt")
      {
        write.table(response_matrix, paste("door_response_matrix",file.format,sep="") )
        write.table(responseRange, paste("door_response_range",file.format,sep="") )
        write.table(unglobalNorm_RM, paste("door_response_matrix_non_normalized",file.format,sep="") )
        write.table(weightGlobNorm, paste("door_global_normalization_weights",file.format,sep="") )
        write.table(odorantReceptors, paste("ORs",file.format,sep="") )
      }
      if (file.format == ".csv") {
        write.csv(response_matrix, paste("door_response_matrix",file.format,sep="") )
        write.csv(responseRange, paste("door_response_range",file.format,sep="") )
        write.csv(unglobalNorm_RM, paste("door_response_matrix_non_normalized",file.format,sep="") )
        write.csv(response_matrix, paste("door_response_matrix",file.format,sep="") )
        write.csv(weightGlobNorm, paste("door_global_normalization_weights",file.format,sep="") )
        write.csv(odorantReceptors, paste("ORs",file.format,sep="") )
      }
    }


  }
