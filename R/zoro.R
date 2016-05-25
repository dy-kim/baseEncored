.onAttach <- function(libname, pkgname) {
  ver <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                  fields = "Version")
  packageStartupMessage("\nPackage ", pkgname, " (", ver, ") loaded.")
}

# To prevent the fault note while 'R CMD check'
globalVariables("V1")
