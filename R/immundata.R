#' ImmunData: A Unified receptors Structure for Immune Receptor receptors
#'
#' `ImmunData` is an abstract class for managing and processing
#' immune receptor repertoire receptors, supporting flexible backends and
#' efficient receptors transformations.
#'
#' @field .receptors description
#' @field .annotations description
#' @field receptor_schema description
#' @field repertoire_schema description
#'
#' @importFrom R6 R6Class
#' @export
ImmunData <- R6Class(
  "ImmunData",
  public = list(
    .receptors = NULL,
    .annotations = NULL,
    receptor_schema = NULL,
    repertoire_schema = NULL,

    #' @description Initializes an `ImmunData` object.
    #'
    #' @param receptors description
    #' @param annotations description
    #' @param schema description
    initialize = function(receptors,
                          annotations,
                          schema) {
      self$.receptors <- receptors
      self$.annotations <- annotations
      self$receptor_schema <- schema

      # TODO: where was the schema built - inside or outside ImmunData? This is related to caching.
      # TODO: when do we assume the correct input?
      # NOTE: receptor building loses some information (puts it into the annotations).
      # - repertoire building does not remove any information
    },

    #' @description Define how this dataset groups receptors to repertoires.
    #'
    #' @param columns description
    #' @param sep description
    build_repertoires = function(schema = "repertoire_id", sep = "-") {
      checkmate::check_character(schema)
      checkmate::check_character(sep)

      missing_cols <- setdiff(schema, colnames(self$annotations))
      if (length(missing_cols) > 0) {
        stop("Missing columns in `annotations`: ",
             paste(missing_cols, collapse = ", "))
      }

      rep_col <- IMD_SCHEMA$repertoire

      self$.annotations <- self$annotations %>%
        dplyr::mutate(
          !!rep_col := dplyr::across(dplyr::all_of(schema)) |>
            dplyr::transmute(.repertoire_id = do.call(paste, c(., sep = sep))) |>
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

      filtered_data <- self$receptors %>% dplyr::filter(!!!filters)

      receptor_ids <- filtered_data %>% dplyr::pull(!!receptor_sym)

      filtered_annot <- self$annotations %>%
        dplyr::filter(!!receptor_sym %in% receptor_ids)

      ImmunData$new(
        receptors = filtered_data,
        annotations = filtered_annot,
        schema = self$receptor_schema
      )
    },

    # TODO: filter by hamming / levenshtein
    # TODO: filter by regex
    # TODO: filter by length

    #' @description Filters rows based on conditions.
    filter_annot = function(...) {
      receptor_sym <- rlang::sym(IMD_SCHEMA$receptor)

      # Capture filter expression for annotations
      filters <- rlang::enquos(...)

      # Step 1: Filter annotations
      filtered_annot <- self$annotations %>% dplyr::filter(!!!filters)

      # Step 2: Get receptor IDs from filtered annotations
      receptor_ids <- filtered_annot %>% dplyr::pull(!!receptor_sym)

      # Step 3: Filter receptor receptors by those receptor IDs
      filtered_data <- self$receptors %>%
        dplyr::filter(!!receptor_sym %in% receptor_ids)

      ImmunData$new(
        receptors = filtered_data,
        annotations = filtered_annot,
        schema = self$receptor_schema
      )
    },

    #' @description Filters rows based on conditions.
    #'
    #' @param barcodes description
    filter_barcodes = function(barcodes = c()) {
      checkmate::check_character(barcodes, .min.len = 1)

      barcode_col <- IMD_SCHEMA$barcode
      receptor_col <- IMD_SCHEMA$receptor

      if (length(barcodes) == 0) {
        warning("No barcodes provided to filter_barcodes(); returning original object.")
        return(self)
      }

      filtered_annot <- self$annotations %>%
        dplyr::filter(!!barcode_sym %in% barcodes)

      receptor_ids <- filtered_annot %>%
        dplyr::pull(!!receptor_sym)

      filtered_data <- self$receptors %>%
        dplyr::filter(!!receptor_sym %in% receptor_ids)

      ImmunData$new(
        receptors = filtered_data,
        annotations = filtered_annot,
        schema = self$receptor_schema
      )
    },

    # TODO: filter_repertoires

    #' @description Counts occurrences of groupings.
    #' @param ... Variables to count.
    #' @return A new `ImmunData` object with count summary.
    count = function(...) {
      private$create_instance(receptors = private$dataset %>% count(...),
                              .metadata = private$metadata)
    },

    #' @description Extracts sample-specific receptors.
    #' @param .sample The sample identifier.
    #' @return A filtered dataset containing only the selected sample.
    `[[` = function(.sample) {
      check_character(.sample)
      private$dataset |> filter(Sample == .sample)
    }
  ),

  active = list(

    #' @description A short description...
    #'
    receptors = function() {
      self$.receptors
    },

    #' @description A short description...
    #'
    annotations = function() {
      self$.annotations
    }
  )
)

#' @exportS3Method dplyr::compute
compute.ImmunData <- function(immdata, ...) {
  checkmate::check_r6(immdata, ImmunData)

  ImmunData$new(
    receptors = immdata$receptors |> compute(),
    annotations = immdata$annotations |> compute(),
    schema = immdata$receptor_schema
  )
}

#' @exportS3Method dplyr::collect
collect.ImmunData <- function(immdata, ...) {
  checkmate::check_r6(immdata, ImmunData)

  ImmunData$new(
    receptors = immdata$receptors |> collect(),
    annotations = immdata$annotations |> collect(),
    schema = immdata$receptor_schema
  )
}
