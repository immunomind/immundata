#' Backend Engine Registry
#'
#' A registry for backend engines used in data processing. This class allows for
#' the registration and retrieval of backend engine instances, ensuring that only
#' valid engines are used within the application.
#'
#' @name BackendEngineRegistry
#' @export
#' @examples
#' registry <- BackendEngineRegistry$new()
#' registry$register("myEngine", MyBackendEngine$new())
#' immloader <- ImmunDataLoder("path/to/yout/dataset")
#' immdata <- ImmunDataLoader$load("myEngine")
#' immdata <- ImmunDataLoader$load(MyBackendEngine$new()) # same as above
#' @keywords internal
BackendEngineRegistry <- R6Class("BackendEngineRegistry",
  private = list(
    engines = list()
  ),
  public = list(

    #' @description Object Constructor
    #' @details Initializes the BackendEngineRegistry object.
    initialize = function() {},

    #' @description Get a list of engine names
    #' @return Character vector of engine names.
    names = function() {
      names(private$engines)
    },

    #' @description Register a new backend engine.
    #' @param .name The name of the backend engine to register. Must be a non-empty string.
    #' If provided a vector, use every elements as an alias.
    #' @param .engine An instance of the backend engine to register. Must be a child instance of an R6 class "BackendEngine"
    #' @return Invisible. The function is called for its side effect of registering an engine.
    #' @examples
    #' registry$register("exampleEngine", ExampleEngine$new())
    register = function(.name, .engine) {
      assertCharacter(.name, min.chars = 1)
      assertR6(.engine, classes = "BackendEngine")

      for (engine_alias in .name) {
        private$engines[[engine_alias]] <- .engine
      }
    },

    #' @description Retrieve a registered backend engine by name.
    #' @param .name The name of the backend engine to retrieve. Must be a non-empty string.
    #' @param ... Other parameters you pass to the constructor of BackendEngine coded by ".name"
    #' @return An instance of the backend engine.
    #' @examples
    #' engine <- registry$getEngine("exampleEngine")
    get = function(.name, ...) {
      assertCharacter(.name, len = 1, min.chars = 1)

      if (!is.null(private$.engines[[.name]])) {
        return(private$.engines[[.name]]$new(...))
      } else {
        stop("Engine not found: ", .name)
      }
    }
  )
)
