IMD_SCHEMA <- list(
  barcode = "imd_barcode", receptor = "imd_receptor_id", repertoire = "imd_repertoire_id"
)



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
  public = list(
    data = NULL,
    annot = NULL,
    receptor_schema = NULL,
    repertoire_schema = NULL,

    #' @description Initializes an `ImmunData` object.
    #' @param .backend The backend storage format for the dataset.
    #' @param .metadata The metadata associated with the dataset.
    initialize = function(.dataset, .annotations, .schema) {
      public$data <- .dataset
      public$annot <- .annotations
      public$receptor_schema <- .schema
    },

    #' @description Define how this dataset groups receptors to repertoires.
    set_repertoires = function(.columns = "repertoire_id", .sep = "-") {
      checkmate::check_character(.columns)
      checkmate::check_character(.sep)

      missing_cols <- setdiff(.columns, colnames(self$annot))
      if (length(missing_cols) > 0) {
        stop("Missing columns in `annot`: ", paste(missing_cols, collapse = ", "))
      }

      rep_col <- IMD_SCHEMA$repertoire

      self$annot <- self$annot %>%
        dplyr::mutate(!!rep_col := dplyr::across(dplyr::all_of(.columns)) |>
                        dplyr::transmute(.repertoire_id = do.call(paste, c(., sep = .sep))) |>
                        dplyr::pull(.repertoire_id)
        )

      invisible(self)
    },

    #' @description Prints the class information.
    print = function() {
      class(self)
    },

    #' @description Filters rows based on conditions.
    #' @param ... Filter conditions.
    #' @return A new `ImmunData` object with filtered rows.
    filter_data = function(...) {
      receptor_sym <- rlang::sym(IMD_SCHEMA$receptor)
      barcode_sym <- rlang::sym(IMD_SCHEMA$barcode)

      filters <- rlang::enquos(...)

      filtered_data <- self$data %>% dplyr::filter(!!!filters)

      receptor_ids <- filtered_data %>% dplyr::pull(!!receptor_sym)

      filtered_annot <- self$annot %>%
        dplyr::filter(!!receptor_sym %in% receptor_ids)

      ImmunData$new(
        .dataset = filtered_data,
        .annotations = filtered_annot,
        .schema = self$receptor_schema
      )
    },

    # TODO: filter by hamming / levenshtein
    # TODO: filter by regex
    # TODO: filter by length

    #' @description Filters rows based on conditions.
    filter_annot = function(...) {
      receptor_sym <- rlang::sym(IMD_SCHEMA$receptor)

      # Capture filter expression for annot
      filters <- rlang::enquos(...)

      # Step 1: Filter annotations
      filtered_annot <- self$annot %>% dplyr::filter(!!!filters)

      # Step 2: Get receptor IDs from filtered annot
      receptor_ids <- filtered_annot %>% dplyr::pull(!!receptor_sym)

      # Step 3: Filter receptor data by those receptor IDs
      filtered_data <- self$data %>%
        dplyr::filter(!!receptor_sym %in% receptor_ids)

      ImmunData$new(
        .dataset = filtered_data,
        .annotations = filtered_annot,
        .schema = self$receptor_schema
      )
    },

    #' @description Filters rows based on conditions.
    filter_barcodes = function(.barcodes = c()) {
      checkmate::check_character(.barcodes, .min.len = 1)

      barcode_col <- IMD_SCHEMA$barcode
      receptor_col <- IMD_SCHEMA$receptor

      if (length(.barcodes) == 0) {
        warning("No barcodes provided to filter_barcodes(); returning original object.")
        return(self)
      }

      filtered_annot <- self$annot %>%
        dplyr::filter(!!barcode_sym %in% .barcodes)

      receptor_ids <- filtered_annot %>%
        dplyr::pull(!!receptor_sym)

      filtered_data <- self$data %>%
        dplyr::filter(!!receptor_sym %in% receptor_ids)

      ImmunData$new(
        .dataset = filtered_data,
        .annotations = filtered_annot,
        .schema = self$receptor_schema
      )
    },

    # TODO: filter_repertoires

    #' @description Counts occurrences of groupings.
    #' @param ... Variables to count.
    #' @return A new `ImmunData` object with count summary.
    count = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% count(...),
        .metadata = private$metadata
      )
    },

    #' @description Extracts sample-specific data.
    #' @param .sample The sample identifier.
    #' @return A filtered dataset containing only the selected sample.
    `[[` = function(.sample) {
      check_character(.sample)
      private$dataset |> filter(Sample == .sample)
    },
  )
)

#' @exportS3Method dplyr::compute
compute.ImmunData <- function(.immdata, ...) {
  .immdata$compute(...)
}

#' @exportS3Method dplyr::collect
collect.ImmunData <- function(.immdata, ...) {
  .immdata$collect(...)
}
