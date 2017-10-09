#' map_receptor
#'
#' Identifying the source of unknown response data by correlating it agains all
#' DoOR responding units.
#'
#' @param data data frame, containing two columns, one called "odorants" and one
#'   "responses" providing InChIKeys and odorant responses respectively.
#' @param response_matrix output is a numeric vector that contains the Pearson
#'   Correlation Coefficient between given data and selected consensus data in
#' @param nshow numeric, if defined, only this number of results will be
#' @param sub character, a subset of responding units
#'   returned response matrix
#' @param threshold.p numeric, a p-value threshold, only correlations below will be returned
#' @param threshold.cor numeric, a correlation-coefficient threshold, only correlations above will be returned
#' @author Shouwen Ma <\email{shouwen.ma@@uni-konstanz.de}>
#' @author Daniel Münch <\email{daniel.muench@@uni-konstanz.de}>
#' @export
#' @importFrom stats na.omit cor.test
#' @aliases mapReceptor map_receptor
#' @examples
#' # load data
#' load_door_data(nointeraction = TRUE)
#'
#' # pick example data
#' data <- data.frame(odorants  = Or22a$InChIKey,
#'                    responses = Or22a$Hallem.2004.EN)
#' data <- na.omit(data)
#'
#' # find the corresponding receptor / responding unit
#' map_receptor(data = data)
map_receptor <- function(data,
                        response_matrix = door_default_values("door_response_matrix"),
                        sub,
                        threshold.p,
                        threshold.cor,
                        nshow) {
  if(!("odorants" %in% colnames(data)) | !("responses" %in% colnames(data)))
       stop("Please provide InChIKeys and responses in columns named 'odorants' and 'responses'.")

  data$odorants   <- as.character(data$odorants)
  data            <- na.omit(data)
  res             <- data.frame()
  response_matrix <- response_matrix[match(data$odorants, rownames(response_matrix)), ]

  if(!missing(sub))
    response_matrix <- response_matrix[ ,match(sub, colnames(response_matrix))]

  # remove n < 3
  n <- which(apply(!is.na(response_matrix), 2, sum) < 3)
  if(length(n) > 0) {
    message(paste("skipped ", paste(names(response_matrix)[n], collapse = ", "), " as overlap (n) was < 3", sep=""))
    response_matrix <- response_matrix[ , -n]
  }

  result <- apply(response_matrix, 2, function(x) cor.test(x, data$responses))

  result <- data.frame(responding.unit = names(result),
                       n               = apply(!is.na(response_matrix), 2, sum),
                       cor             = unlist(sapply(result, "[","estimate")),
                       p.value         = unlist(sapply(result, "[","p.value")))

  result <- result[order(result$cor, decreasing = TRUE),]

  if(!missing(threshold.p))
    result <- result[which(result$p.value <= threshold.p),]

  if(!missing(threshold.cor))
    result <- result[which(result$cor >= threshold.cor),]

  if(!missing(nshow))
    result <- result[1:nshow, ]

  return(result)
}
