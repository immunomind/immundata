#' Backend Engine Registry
#'
#' A registry for backend backends used in data processing. This class allows for
#' the registration and retrieval of backend engine instances, ensuring that only
#' valid backends are used within the application.
#'
#' @name BackendRegistry
#' @export
#' @keywords internal
BackendRegistry <- R6Class("BackendRegistry",
  private = list(
    backends = list()
  ),
  public = list(

    #' @description Object Constructor
    #' @details Initializes the BackendRegistry object.
    initialize = function() {},

    #' @description Get a list of engine names
    #' @return Character vector of engine names.
    names = function() {
      names(private$backends)
    },

    #' @description Register a new backend engine.
    #' @param .name The name of the backend engine to register. Must be a non-empty string.
    #' If provided a vector, use every elements as an alias.
    #' @param .backend An instance of the backend engine to register. Must be a child instance of an R6 class "Backend"
    #' @return Invisible. The function is called for its side effect of registering an engine.
    register = function(.name, .backend) {
      assert_character(.name, min.chars = 1)
      assert_r6_gen(.backend, classes = "ImmunData")

      for (engine_alias in .name) {
        # TODO: check for the engines which use the same alias
        private$backends[[engine_alias]] <- .backend
      }
    },

    #' @description Retrieve a registered backend engine by name.
    #' @param .name The name of the backend engine to retrieve. Must be a non-empty string.
    #' @param ... Other parameters you pass to the constructor of Backend coded by ".name"
    #' @return An instance of the backend engine.
    get = function(.name) {
      assert_character(.name, len = 1, min.chars = 1)

      if (!is.null(private$backends[[.name]])) {
        return(private$backends[[.name]])
      } else {
        stop("Engine not found: ", .name)
      }
    }
  )
)
