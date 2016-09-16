.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("\nWelcome to ", utils::packageDescription(pkgname)$Package,
                               "\nVersion: "  , utils::packageDescription(pkgname)$Version,
                               "\nReleased: " , utils::packageDescription(pkgname)$Date,
                               "\n\nPlease note that all function names have been changed to 'snake_case' for consistency. 
                                \nOld  'camelCase' function names will stop working in one of the next versions."
                               ))
}


# transition to new function names  ---------------------------------------

# exported functions
#' @export
countStudies <- count_studies
