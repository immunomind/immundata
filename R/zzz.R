.onLoad <- function(libname, pkgname) {
  message("Loading immundata version ", packageVersion(pkgname))

  # Do registry stuff here
  .BACKEND_REGISTRY_CLASS <- BackendEngineRegistry$new()
  .BACKEND_REGISTRY_CLASS$register(c("df", "data.frame"), BackendEngine)

  loader <- ImmunDataLoader("./", "df")
}

.onUnload <- function(libpath) {
  message("Unloading immundata")

  # Cleanup created datasets, close connection
}

get_available_engines <- function() {
  .BACKEND_REGISTRY_CLASS$engines()
}
