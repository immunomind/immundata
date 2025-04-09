#' @importFrom R6 R6Class
#'
#' @title ImmunData: A Unified Structure for Immune Receptor Repertoire Data
#'
#' @description
#' `ImmunData` is an abstract R6 class for managing and transforming immune receptor repertoire data.
#' It supports flexible backends (e.g., Arrow, DuckDB, dbplyr) and lazy evaluation,
#' and provides tools for filtering, aggregation, and receptor-to-repertoire mapping.
#'
#' @seealso [read_repertoires()], [read_immundata()]
#'
#' @export
ImmunData <- R6Class(
  "ImmunData",
  private = list(
    # .receptors A receptor-level table containing immune receptor features
    # (e.g., CDR3, V/J gene, clonotype ID, counts). This table is typically aggregated
    # and is used for quantitative analysis of immune repertoire signatures.
    # It can be a local tibble, Arrow Table, DuckDB table, or any other
    # `dplyr`-compatible backend (including lazy data sources).
    .receptors = NULL,

    # .annotations A barcode-level table that links each barcode (i.e., cell ID)
    # to a receptor in `.receptors`. It can also store cell-level metadata such as
    # sample ID, donor, or tissue source. This table is **not aggregated** and
    # typically contains one row per barcode.
    .annotations = NULL,

    # .repertoire_table A duckplyr table with repertoire names and receptor counts.
    .repertoire_table = NULL
  ),
  public = list(

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
    #' @param repertoires A repertoire table, created inside the body of [agg_repertoires].
    initialize = function(receptors,
                          annotations,
                          schema,
                          repertoires = NULL) {
      private$.receptors <- receptors
      private$.annotations <- annotations
      self$schema_receptor <- schema

      if (!is.null(repertoires)) {
        self$schema_repertoire <- setdiff(colnames(repertoires), "n")
        private$.repertoire_table <- repertoires
      }
    }
  ),
  active = list(

    #' @field receptors Accessor for the receptor-level table (`.receptors`).
    receptors = function() {
      private$.receptors
    },

    #' @field annotations Accessor for the annotation-level table (`.annotations`).
    annotations = function() {
      private$.annotations
    },

    #' @field barcodes Get a vector of barcodes from the annotation table.
    barcodes = function() {
      barcode_col_id <- imd_schema()$barcode
      private$.annotations |> select({{ barcode_col_id }})
    },

    #' @field repertoires Get a vector of repertoire names after data aggregation with [agg_repertoires()]
    repertoires = function() {
      # TODO: cache repertoire table to memory if not very big?
      if (!is.null(private$.repertoire_table)) {
        private$.repertoire_table |> collect()
      } else {
        warning("No repertoires aggregated for this ImmunData. See `?agg_repertoires` for more information.")
        NULL
      }
    }
  )
)
