.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("\nWelcome to ", utils::packageDescription(pkgname)$Package,
                               "\nVersion: "  , utils::packageDescription(pkgname)$Version,
                               "\nReleased: " , utils::packageDescription(pkgname)$Date,
                               "\n\nRemember to also load DoOR.data.\n"
  ))
}