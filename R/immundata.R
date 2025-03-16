#' ImmunData: A Unified Data Structure for Immune Receptor Data
#'
#' `ImmunData` is an abstract class for managing and processing
#' immune receptor repertoire data, supporting flexible backends and
#' efficient data transformations.
#'
#' @section Methods:
#' - `$initialize(.backend, .metadata)`: Creates a new `ImmunData` object.
#' - `$to_list()`: Converts the dataset into a list.
#' - `$print()`: Prints the class information.
#' - `$arrange(...)`: Arranges rows by given variables.
#' - `$filter(...)`: Filters rows based on conditions.
#' - `$slice(...)`: Selects specific rows by index.
#' - `$slice_head(...)`: Selects the first rows of the dataset.
#' - `$slice_tail(...)`: Selects the last rows of the dataset.
#' - `$mutate(...)`: Modifies or adds columns.
#' - `$select(...)`: Selects specific columns.
#' - `$count(...)`: Counts occurrences of groupings.
#' - `$group_by(...)`: Groups data by variables.
#' - `$summarise(...)`: Aggregates data.
#' - `$compute(...)`: Computes intermediate results.
#' - `$collect(...)`: Collects data into memory.
#' - `$[[(.sample)`: Extracts sample-specific data.
#' - `$data()`: Retrieves the stored dataset.
#'
#' @importFrom R6 R6Class
#' @export
ImmunData <- R6Class(
  "ImmunData",
  private = list(
    backend = NULL,
    metadata = NULL,
    repertoire_model = NULL,
    clonotype_model = NULL,
    repertoire_names = NULL,

    create_instance = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      stop(MESSAGES$NotImpl)
    }
  ),
  public = list(

    #' @description Initializes an `ImmunData` object.
    #' @param .backend The backend storage format for the dataset.
    #' @param .metadata The metadata associated with the dataset.
    initialize = function(.backend, .metadata) {
      private$backend <- .backend
      private$metadata <- .metadata
    },

    #' @description Converts the dataset into a list.
    #' @return A named list containing `data` and `meta` components.
    to_list = function() {
      dataset <- private$dataset %>% group_split()
      names(dataset) <- private$repertoire_names
      metadata <- NULL
      list(data = dataset, meta = metadata)
    },

    #' @description Prints the class information.
    print = function() {
      class(self)
    },

    #' @description Arranges rows by given variables.
    #' @param ... Columns to sort by.
    #' @return A new `ImmunData` object with arranged rows.
    arrange = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% arrange(...),
        .metadata = private$metadata
      )
    },

    #' @description Filters rows based on conditions.
    #' @param ... Filter conditions.
    #' @return A new `ImmunData` object with filtered rows.
    filter = function(...) {
      ImmunData$new(private$backend.filter(...), metadata)
    },

    #' @description Selects specific rows by index.
    #' @param ... Row indices.
    #' @return A new `ImmunData` object with selected rows.
    slice = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% slice(...),
        .metadata = private$metadata
      )
    },

    #' @description Selects the first rows of the dataset.
    #' @param ... Number of rows to return.
    #' @return A new `ImmunData` object with the first rows.
    slice_head = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% slice_head(...),
        .metadata = private$metadata
      )
    },

    #' @description Selects the last rows of the dataset.
    #' @param ... Number of rows to return.
    #' @return A new `ImmunData` object with the last rows.
    slice_tail = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% slice_tail(...),
        .metadata = private$metadata
      )
    },

    #' @description Modifies or adds columns.
    #' @param ... Column transformations.
    #' @return A new `ImmunData` object with modified columns.
    mutate = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% mutate(...),
        .metadata = private$metadata
      )
    },

    #' @description Selects specific columns.
    #' @param ... Columns to retain.
    #' @return A new `ImmunData` object with selected columns.
    select = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% select(...),
        .metadata = private$metadata
      )
    },

    #' @description Counts occurrences of groupings.
    #' @param ... Variables to count.
    #' @return A new `ImmunData` object with count summary.
    count = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% count(...),
        .metadata = private$metadata
      )
    },

    #' @description Groups data by variables.
    #' @param ... Variables to group by.
    #' @return A new `ImmunData` object with grouped data.
    group_by = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% group_by(...),
        .metadata = private$metadata
      )
    },

    #' @description Aggregates data.
    #' @param ... Aggregation expressions.
    #' @return A new `ImmunData` object with summarized data.
    summarise = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% summarise(...),
        .metadata = private$metadata
      )
    },

    #' @description Computes intermediate results.
    #' @param ... Additional arguments passed to the compute function.
    #' @return A new `ImmunData` object with computed results.
    compute = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% compute(...),
        .metadata = private$metadata
      )
    },

    #' @description Collects data into memory.
    #' @param ... Additional arguments passed to the collect function.
    #' @return The collected dataset.
    collect = function(...) {
      private$dataset %>% collect(...)
    },

    #' @description Extracts sample-specific data.
    #' @param .sample The sample identifier.
    #' @return A filtered dataset containing only the selected sample.
    `[[` = function(.sample) {
      check_character(.sample)
      private$dataset |> filter(Sample == .sample)
    },

    #' @description Retrieves the stored dataset.
    #' @return The dataset stored in the object.
    data = function() {
      private$dataset
    }
  )
)


####
## Row operations
####

#' @exportS3Method dplyr::arrange
arrange.ImmunData <- function(.immdata, ...) {
  .immdata$arrange(...)
}

#' @exportS3Method dplyr::filter
filter.ImmunData <- function(.immdata, ...) {
  .immdata$filter(...)
}

#' @exportS3Method dplyr::slice
slice.ImmunData <- function(.immdata, ...) {
  .immdata$slice(...)
}

#' @exportS3Method dplyr::slice_head
slice_head.ImmunData <- function(.immdata, ...) {
  .immdata$slice_head(...)
}

#' @exportS3Method dplyr::slice_tail
slice_tail.ImmunData <- function(.immdata, ...) {
  .immdata$slice_tail(...)
}

####
## Column operations
####

#' @exportS3Method dplyr::mutate
mutate.ImmunData <- function(.immdata, ...) {
  .immdata$mutate(...)
}

#' @exportS3Method dplyr::select
select.ImmunData <- function(.immdata, ...) {
  .immdata$select(...)
}

####
## Group operations
####

#' @exportS3Method dplyr::count
count.ImmunData <- function(.immdata, ...) {
  .immdata$count(...)
}

#' @exportS3Method dplyr::group_by
group_by.ImmunData <- function(.data, ...) {
  .data$group_by(...)
}

#' @exportS3Method dplyr::summarise
summarise.ImmunData <- function(.immdata, ...) {
  .immdata$summarise(...)
}

#' @exportS3Method dplyr::summarize
summarize.ImmunData <- function(.immdata, ...) {
  .immdata$summarise(...)
}

####
## Data operations
####

#' @exportS3Method dplyr::compute
compute.ImmunData <- function(.immdata, ...) {
  .immdata$compute(...)
}

#' @exportS3Method dplyr::collect
collect.ImmunData <- function(.immdata, ...) {
  .immdata$collect(...)
}


#' DataFrameImmunData: ImmunData Representation Using Tibble
#'
#' This class extends `ImmunData` and represents immune receptor repertoire data
#' using `tibble` as the internal data structure.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL)}}{
#'     Creates a new `DataFrameImmunData` object.
#'   }
#' }
#'
#' @param .dataset A data frame containing immune receptor data.
#' @param .metadata A data frame containing metadata information.
#' @param .repertoire_model Optional. A model for repertoire-level analysis.
#' @param .clonotype_model Optional. A model for clonotype-level analysis.
#'
#' @return An object of class `DataFrameImmunData`.
#'
#' @importFrom tibble as_tibble
#' @export
DataFrameImmunData <- R6Class(
  "DataFrameImmunData",
  inherit = ImmunData,
  private = list(
    create_instance = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      DataFrameImmunData$new(.dataset = .dataset, .metadata = .metadata)
    }
  ),
  public = list(

    #' @description Initializes a `DataFrameImmunData` object.
    #' @param .dataset A data frame containing immune receptor data.
    #' @param .metadata A data frame containing metadata information.
    #' @param .repertoire_model Optional. A model for repertoire-level analysis.
    #' @param .clonotype_model Optional. A model for clonotype-level analysis.
    initialize = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      private$dataset <- as_tibble(.dataset)
      private$metadata <- as_tibble(.metadata)
    }
  )
)


#' DataTableImmunData: ImmunData Representation Using tidytable
#'
#' This class extends `ImmunData` and represents immune receptor repertoire data
#' using `tidytable` as the internal data structure.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL)}}{
#'     Creates a new `DataTableImmunData` object.
#'   }
#' }
#'
#' @param .dataset A data frame containing immune receptor data.
#' @param .metadata A data frame containing metadata information.
#' @param .repertoire_model Optional. A model for repertoire-level analysis.
#' @param .clonotype_model Optional. A model for clonotype-level analysis.
#'
#' @return An object of class `DataTableImmunData`.
#'
#' @importFrom tidytable as_tidytable
#' @export
DataTableImmunData <- R6Class(
  "DataTableImmunData",
  inherit = ImmunData,
  private = list(
    create_instance = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      DataTableImmunData$new(.dataset = .dataset, .metadata = .metadata)
    }
  ),
  public = list(

    #' @description Initializes a `DataTableImmunData` object.
    #' @param .dataset A data frame containing immune receptor data.
    #' @param .metadata A data frame containing metadata information.
    #' @param .repertoire_model Optional. A model for repertoire-level analysis.
    #' @param .clonotype_model Optional. A model for clonotype-level analysis.
    initialize = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      private$dataset <- as_tidytable(.dataset)
      private$metadata <- as_tibble(.metadata)
    }
  )
)
