.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("\nWelcome to ",  packageDescription(pkgname)$Package,
                               "\nVersion: ",  packageDescription(pkgname)$Version,
                               "\nReleased: ", packageDescription(pkgname)$Date,
                               "\n\nRemember to also load DoOR.data.\n"
  ))
}