.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loading immundata version ", packageVersion(pkgname))
}

.onUnload <- function(libpath) {
  message("Unloading immundata")
}
