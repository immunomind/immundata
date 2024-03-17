#' ImmunDataLoader
#'
#' A class for loading immunological data. This loader supports different backend
#' engines for data processing and can handle multiple data formats.
#'
#' @name ImmunDataLoader
#' @export
#' @examples
#' loader <- ImmunDataLoader$new(".path/to/data", .engine = "df")
#' data_list <- loader$list()
#' data <- loader$load("specific_data_file.csv")
#' all_data <- loader$load_all()
ImmunDataLoader <- R6Class(
  "ImmunDataLoader",
  private = list(
    source_path = "",
    engine = NULL,
    scan_files = function(.format = NA) {
      # Placeholder: Implement logic to get all files recursively based on the specified format.
      # This can use list.files() with pattern argument if .format is not NA
      # Return a vector of file paths
    }
  ),
  public = list(

    #' @description Constructor for ImmunDataLoader
    #' @details Initializes a new ImmunDataLoader object with a specified source path and backend engine.
    #' @param .path Character; the path to the directory containing data files.
    #' @param .engine Character or BackendEngine; the backend engine to use for data processing. Default is "df".
    #' @param .format Character; the format of the data files to load. This parameter is optional and defaults to NA.
    initialize = function(.path, .engine = "df", .format = NA) {
      assertCharacter(.path, min.len = 1)
      assert(
        checkChoice(.engine, get_available_engines()),
        checkR6(.engine, "BackendEngine")
      )
      assert(
        checkCharacter(.format),
        checkScalarNA(.format)
      )


      private$source_path <- .path
      # Assuming a function getEngine or similar logic to resolve .engine
      # to a BackendEngine object if .engine is a character string.
      # For simplicity, this placeholder does not implement the actual retrieval.
      if (is.character(.engine)) {
        # Example: private$engine <- BackendEngineRegistry$getEngine(.engine)
        private$engine <- .engine # Placeholder
      } else {
        private$engine <- .engine
      }
      # Optionally call private$scan_files(.format) to initialize file listing
    },

    #' @description Finalizer for ImmunDataLoader
    #' @details Cleans up resources when the object is garbage collected.
    finalize = function() {
      # Placeholder: Clean up resources if needed, such as closing connections.
    },

    #' @description List Data Files
    #' @details Lists all data files available in the source directory, optionally filtered by format.
    #' @return Character vector of the file names available for loading.
    list = function() {
      # Use the private$scan_files method to list available data files.
      # Return the list to the caller.
      return(private$scan_files())
    },

    #' @description Load a Specific Data File
    #' @details Loads a specific data file by name using the configured backend engine.
    #' @param .filename Character; the name of the file to load.
    #' @return Data frame or other structure containing the loaded data, depending on the backend engine.
    load = function(.filename) {
      assertCharacter(.filename, min.len = 1, add = "Filename must be a non-empty string.")

      # Implement logic to load a single file specified by filename.
      # This method would use the backend engine to process the data.
      # Placeholder implementation:
      # filePath <- file.path(private$source_path, filename)
      # data <- read.csv(filePath) # Simplified example; actual implementation depends on the engine
      # return(data)
    },

    #' @description Load All Data Files
    #' @details Loads all available data files using the configured backend engine.
    #' @return List of data frames or other structures containing the loaded data, depending on the backend engine.
    load_all = function() {
      # Load all files found by private$scan_files and process them with the engine.
      # Placeholder: Iterate over files returned by private$scan_files and load each.
      # Return a list or combined dataset of all loaded data.
    }
  )
)
