#' @title ImmunData: A Unified Structure for Immune Receptor Repertoire Data
#'
#' @description
#' `ImmunData` is an abstract R6 class for managing and transforming immune receptor repertoire data.
#' It supports flexible backends (e.g., Arrow, DuckDB, dbplyr) and lazy evaluation,
#' and provides tools for filtering, aggregation, and receptor-to-repertoire mapping.
#'
#' @importFrom R6 R6Class
#' @export
ImmunData <- R6Class(
  "ImmunData",
  public = list(

    #' @field .receptors A receptor-level table containing immune receptor features
    #'   (e.g., CDR3, V/J gene, clonotype ID, counts). This table is typically aggregated
    #'   and is used for quantitative analysis of immune repertoire signatures.
    #'   It can be a local tibble, Arrow Table, DuckDB table, or any other
    #'   `dplyr`-compatible backend (including lazy data sources).
    .receptors = NULL,

    #' @field .annotations A barcode-level table that links each barcode (i.e., cell ID)
    #'   to a receptor in `.receptors`. It can also store cell-level metadata such as
    #'   sample ID, donor, or tissue source. This table is **not aggregated** and
    #'   typically contains one row per barcode.
    .annotations = NULL,

    #' @field schema_receptor A named list describing how to interpret receptor-level data.
    #'   This includes the fields used for aggregation (e.g., `CDR3`, `V_gene`, `J_gene`),
    #'   and optionally unique identifiers for each receptor row. Used to ensure consistency
    #'   across processing steps.
    schema_receptor = NULL,

    #' @field schema_repertoire A named list defining how barcodes or annotations should be
    #'   grouped into repertoires. This may include sample-level metadata (e.g., `sample_id`,
    #'   `donor_id`) used to define unique repertoires.
    schema_repertoire = NULL,

    #' @description Creates a new `ImmunData` object.
    #' This constructor expects receptor-level and barcode-level data,
    #' along with a receptor schema defining aggregation and identity fields.
    #'
    #' @param receptors A receptor-level dataset (e.g., grouped by CDR3/V/J).
    #' @param annotations A cell/barcode-level dataset mapping barcodes to receptor rows.
    #' @param schema A named list specifying the receptor schema (e.g., aggregate fields, ID columns).
    initialize = function(receptors,
                          annotations,
                          schema) {
      self$.receptors <- receptors
      self$.annotations <- annotations
      self$schema_receptor <- schema

      # TODO: where was the schema built - inside or outside ImmunData? This is related to caching.
      # TODO: when do we assume the correct input?
      # NOTE: receptor building loses some information (puts it into the annotations).
      # - repertoire building does not remove any information
    },

    #' @description Defines repertoires by concatenating selected annotation columns.
    #'
    #' @param schema A character vector of column names in `.annotations` used to define repertoire grouping.
    #' @param sep A string separator used to concatenate multiple columns into a single repertoire ID.
    build_repertoires = function(schema = "repertoire_id", sep = "-") {
      checkmate::check_character(schema)
      checkmate::check_character(sep)

      missing_cols <- setdiff(schema, colnames(self$annotations))
      if (length(missing_cols) > 0) {
        stop(
          "Missing columns in `annotations`: ",
          paste(missing_cols, collapse = ", ")
        )
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

    #' @description Prints class information for the `ImmunData` object.
    print = function() {
      class(self)
    },

    #' @description Filters the receptor-level data using tidyverse filter syntax,
    #' and then updates the annotation table to include only linked barcodes.
    #'
    #' @param ... Filtering conditions applied to the receptor-level table.
    #' @return A new `ImmunData` object with filtered receptor and annotation tables.
    filter_receptors = function(...) {
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
        schema = self$schema_receptor
      )
    },

    # TODO: filter by hamming / levenshtein
    # TODO: filter by regex
    # TODO: filter by length

    #' @description Filters the annotation-level table using tidyverse filter syntax,
    #' and updates the receptor table to include only matching receptor entries.
    #'
    #' @param ... Filtering conditions applied to the annotations table.
    #' @return A new `ImmunData` object with filtered annotation and receptor tables.
    filter_annotations = function(...) {
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
        schema = self$schema_receptor
      )
    },

    #' @description Filters the dataset by a set of barcodes (unique cell IDs).
    #' The resulting object contains only rows linked to those barcodes.
    #'
    #' @param barcodes A character vector of barcodes to retain.
    #' @return A new `ImmunData` object filtered by barcode.
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
        schema = self$schema_receptor
      )
    },

    # TODO: filter_repertoires

    #' @description Extracts a sample-specific view of the dataset.
    #'
    #' @param .sample A sample identifier used to filter the receptor data.
    #' @return A subset of the receptor dataset corresponding to the selected sample.
    `[[` = function(.sample) {
      check_character(.sample)
      private$dataset |> filter(Sample == .sample)
    }
  ),
  active = list(

    #' @field receptors Accessor for the receptor-level table (`.receptors`).
    receptors = function() {
      self$.receptors
    },

    #' @field annotations Accessor for the annotation-level table (`.annotations`).
    annotations = function() {
      self$.annotations
    }
  )
)

#' @title Materializes all lazy tables in the `ImmunData` object using `dplyr::compute()`.
#' @param immdata An `ImmunData` object.
#' @param ... Other arguments passed to [dplyr::compute]
#' @return A new `ImmunData` object with computed receptor and annotation tables.
#' @importFrom dplyr compute
compute.ImmunData <- function(immdata, ...) {
  checkmate::check_r6(immdata, ImmunData)

  ImmunData$new(
    receptors = immdata$receptors |> compute(),
    annotations = immdata$annotations |> compute(),
    schema = immdata$schema_receptor
  )
}

#' @title Collects all lazy tables in the `ImmunData` object into memory using `dplyr::collect()`.
#' @param immdata An `ImmunData` object.
#' @param ... Other arguments passed to [dplyr::collect]
#' @return A new `ImmunData` object with in-memory receptor and annotation tables.
#' @importFrom dplyr collect
collect.ImmunData <- function(immdata, ...) {
  checkmate::check_r6(immdata, ImmunData)

  ImmunData$new(
    receptors = immdata$receptors |> collect(),
    annotations = immdata$annotations |> collect(),
    schema = immdata$schema_receptor
  )
}
