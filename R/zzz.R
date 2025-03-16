.immundataglobalenv <- new.env(parent = emptyenv())

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loading immundata version ", packageVersion(pkgname))

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

#' Get Available Backends for ImmunData
#'
#' Returns a list of currently registered backends for `ImmunData`.
#' These backends determine the internal data representation,
#' such as `data.frame`, `data.table`, or other formats.
#'
#' @return A character vector of registered backend names.
#' If no backends are registered, returns an empty character vector.
#'
#' @export
get_available_backends <- function() {
  .immundataglobalenv$backend_registry$names()
}
