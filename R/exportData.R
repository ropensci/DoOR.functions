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
#' data "response.matrix", "response.range", "response.matrix_non.normalized",
#' "response.matrix", "weight.globNorm" and "ORs".
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @export
#' @importFrom utils write.csv write.table
#' @keywords data
#' @examples
#' \dontrun{
#' library(DoOR.data)
#' library(DoOR.functions)
#' load_door_data()
#' exportData(".txt", all.data = FALSE) 	# export odorant responses data only
#' }
exportData <-
  function(file.format, directory,
           odorantReceptors = default.val("ORs"),
           response_matrix = default.val("response.matrix"),
           responseRange = default.val("response.range"),
           unglobalNorm_RM = default.val("response.matrix_non.normalized"),
           weightGlobNorm = default.val("weight.globNorm"),
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
        write.table(response_matrix, paste("response.matrix",file.format,sep="") )
        write.table(responseRange, paste("response.range",file.format,sep="") )
        write.table(unglobalNorm_RM, paste("response.matrix_non.normalized",file.format,sep="") )
        write.table(weightGlobNorm, paste("weight.globNorm",file.format,sep="") )
        write.table(odorantReceptors, paste("ORs",file.format,sep="") )
      }
      if (file.format == ".csv") {
        write.csv(response_matrix, paste("response.matrix",file.format,sep="") )
        write.csv(responseRange, paste("response.range",file.format,sep="") )
        write.csv(unglobalNorm_RM, paste("response.matrix_non.normalized",file.format,sep="") )
        write.csv(response_matrix, paste("response.matrix",file.format,sep="") )
        write.csv(weightGlobNorm, paste("weight.globNorm",file.format,sep="") )
        write.csv(odorantReceptors, paste("ORs",file.format,sep="") )
      }
    }


  }
