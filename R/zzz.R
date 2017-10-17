.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "\nWelcome to ",
      utils::packageDescription(pkgname)$Package,
      "\nVersion: "  ,
      utils::packageDescription(pkgname)$Version,
      "\nReleased: " ,
      utils::packageDescription(pkgname)$Date,
      "\n\nPlease note that all function names have been changed to 'snake_case'
       for consistency.
      \nOld  'camelCase' function names will stop working in one of the next
       versions."
    )
    )
}


# transition to new function names  ---------------------------------------

# exported functions
#' @export
countStudies <- count_studies
backProject <- back_project
calModel <- calculate_model
CreateDatabase <- create_door_database
default.val <- door_default_values
DoOREst <- estimate_missing_value
DoORnorm <- door_norm
dplot_acrossOSNs <- dplot_across_osns
dplot_acrossReceptors <- dplot_across_ru
dplot_ALmap <- dplot_al_map
dplot_compareProfiles <- dplot_compare_profiles
dplot_responseMatrix <- dplot_response_matrix
dplot_responseProfile <- dplot_response_profile
exportData <- export_door_data
getNormalizedResponses <- get_normalized_responses
getResponses <- get_responses
identifySensillum <- identify_sensillum
importNewData <- import_new_data
privateOdorant <- private_odorant
projectPoints <- project_points
removeStudy <- remove_study
resetSFR <- reset_sfr
selectModel <- select_model
transID <- trans_id
modelRPSEQ <- model_response_seq
modelRP <- model_response
updateDatabase <- update_door_database
updateOdorInfo <- update_door_odorinfo
mapReceptor <- map_receptor
