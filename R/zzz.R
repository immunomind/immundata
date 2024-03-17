.onLoad <- function(libname, pkgname) {
  message("Loading immundata version ", packageVersion(pkgname))

  # Do registry stuff here
}

.onUnload <- function(libpath) {
  message("Unloading immundata")

  # Cleanup created datasets, close connection
}
