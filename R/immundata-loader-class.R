#' ImmunDataLoader
#'
#' A class for loading immunological data. This loader supports different backend
#' backends for data processing and can handle multiple data formats.
#'
#' @name ImmunDataLoader
#' @export
#' @examples
#' loader <- ImmunDataLoader$new(".path/to/data", .backend = "df")
#' data_list <- loader$list()
#' data <- loader$load("specific_data_file.csv")
#' all_data <- loader$load_all()
ImmunDataLoader <- R6Class(
  "ImmunDataLoader",
  private = list(
    source_path = "",
    backend = NULL,
    scan_files = function(.format = NA) {
      # Placeholder: Implement logic to get all files recursively based on the specified format.
      # This can use list.files() with pattern argument if .format is not NA
      # Return a vector of file paths
    }
  ),
  public = list(

    #' @description Constructor for ImmunDataLoader
    #' @details Initializes a new ImmunDataLoader object with a specified source path and backend backend.
    #' @param .path Character; the path to the directory containing data files.
    #' @param .backend Character or Backend; the backend backend to use for data processing. Default is "df".
    #' @param .format Character; the format of the data files to load. This parameter is optional and defaults to NA.
    initialize = function(.path, .backend = "df", .recursive = c(TRUE, FALSE), .cache = c(TRUE, FALSE), .format = NA) {
      assert_character(.path, max.len = 1)
      assert_logical(.recursive[1])
      assert_logical(.cache[1])
      assert(
        check_choice(.backend, get_available_backends()),
        check_r6_gen(.backend)
      )
      assert(
        check_character(.format),
        check_scalar_na(.format)
      )

      private$source_path <- .path

      if (is.character(.backend)) {
        private$backend <- .immundataglobalenv$backend_registry$get(.backend)
      } else {
        private$backend <- .backend
      }
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
    #' @details Loads a specific data file by name using the configured backend backend.
    #' @param .filename Character; the name of the file to load. Could be a vector if you want to load several files.
    #' @return Data frame or other structure containing the loaded data, depending on the backend backend.
    load = function(.filename) {
      assert_character(.filename, min.len = 1, add = "Filename must be a non-empty string.")

      library(immunarch)
      data(immdata)
      immdata$data <- lapply(names(immdata$data), function(df_name) immdata$data[[df_name]] |> mutate(Sample = df_name))
      immdata$data <- do.call(rbind, immdata$data)
      imd <- private$backend$new(immdata$data, immdata$meta)

      imd
    },

    #' @description Load All Data Files
    #' @details Loads all available data files using the configured backend backend.
    #' @return List of data frames or other structures containing the loaded data, depending on the backend backend.
    load_all = function() {
      # Load all files found by private$scan_files and process them with the backend.
      # Placeholder: Iterate over files returned by private$scan_files and load each.
      # Return a list or combined dataset of all loaded data.
    }
  )
)
