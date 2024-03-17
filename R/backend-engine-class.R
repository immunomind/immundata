#' Backend Engine
#'
#' A registry for backend engines used in data processing. This class facilitates
#' the registration and retrieval of backend engine instances. By ensuring that only
#' valid engines are utilized within the application, it promotes consistency and reliability
#' in data processing operations. This class plays a crucial role in managing different backend
#' engines, allowing for a modular and extensible architecture in data analysis applications.
#'
#' @name BackendEngine
#' @export BackendEngine
#' @examples
#' engine <- BackendEngine$new()
#' dataset <- engine$create_dataset(data.frame(x = 1:10, y = 11:20))
library(R6)
library(checkmate)

BackendEngine <- R6Class("BackendEngine",
  public = list(

    #' @description Object constructor for BackendEngine.
    #' @details Initializes a new instance of the BackendEngine class.
    initialize = function() {
      # Initialization code can go here.
    },

    #' @description Object destructor for BackendEngine.
    #' @details Finalizes this instance of the BackendEngine class. E.g., closes database connections.
    finalize = function() {
      # Finalization code can go here.
    },

    #' @description Create a new dataset with this engine as a backend computation engine
    #' @details
        #' Processes input data and creates a dataset suitable for analysis.
        #' Currently, this method acts as a placeholder and returns the input data directly.
        #' Future implementations should include data processing logic.
    #' @param source A data frame or similar object to be processed into a dataset.
    #' @return The processed dataset, which is currently the input data unmodified.
    #' @examples
        #' engine <- BackendEngine$new()
        #' dataset <- engine$create_dataset(data.frame(x = 1:10, y = 11:20))
        #' print(dataset)
    create_dataset = function(source) {
      stop(MESSAGES[["NotImpl"]])
    }
  )
)
