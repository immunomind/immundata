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

    #' Object Constructor
    #'
    #' Initializes the BackendEngineRegistry object.
    initialize = function() {},

    #' @description Register a new backend engine.
    #' @param name The name of the backend engine to register. Must be a non-empty string.
    #' @param engine An instance of the backend engine to register. Must be a child instance of an R6 class "BackendEngine"
    #' @return Invisible. The function is called for its side effect of registering an engine.
    #' @examples
    #' registry$register("exampleEngine", ExampleEngine$new())
    register = function(name, engine) {
      assertCharacter(name, len = 1, min.chars = 1, add = "Engine name must be a non-empty string.")
      assertR6(engine, classes = "BackendEngine", add = "Engine must be an instance of BackendEngine.")

      private$engines[[name]] <- engine
    },

    #' @description Retrieve a registered backend engine by name.
    #' @param name The name of the backend engine to retrieve. Must be a non-empty string.
    #' @return An instance of the backend engine.
    #' @examples
    #' engine <- registry$getEngine("exampleEngine")
    get = function(name) {
      assertCharacter(name, len = 1, min.chars = 1, add = "Engine name must be a non-empty string.")

      if (!is.null(private$.engines[[name]])) {
        return(private$.engines[[name]]$new())
      } else {
        stop("Engine not found: ", name)
      }
    }
  )
)
