.immundataglobalenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  message("Loading immundata version ", packageVersion(pkgname))

  # Do registry stuff here
  assign("backend_registry", BackendRegistry$new(), envir = .immundataglobalenv)

  register_backends()
}

.onUnload <- function(libpath) {
  message("Unloading immundata")

  # Cleanup created datasets, close connection
}

register_backends <- function() {
  .immundataglobalenv$backend_registry$register(c("df", "data.frame"), DataFrameImmunData)
  .immundataglobalenv$backend_registry$register(c("dt", "data.table"), DataTableImmunData)
}

#' @export
get_available_backends <- function() {
  .immundataglobalenv$backend_registry$names()
}
