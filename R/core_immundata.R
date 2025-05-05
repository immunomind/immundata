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

    # .annotations A barcode-level table that links each barcode (i.e., cell ID)
    # to a receptor. It can also store cell-level metadata such as
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
    #' @param schema A character vector specifying the receptor schema (e.g., aggregate fields, ID columns).
    #' @param annotations A cell/barcode-level dataset mapping barcodes to receptor rows.
    #' @param repertoires A repertoire table, created inside the body of [agg_repertoires].
    initialize = function(schema,
                          annotations,
                          repertoires = NULL) {
      checkmate::check_data_frame(annotations)

      if (checkmate::test_character(schema)) {
        schema <- make_receptor_schema(features = schema, chains = NULL)
      }

      private$.annotations <- annotations
      self$schema_receptor <- schema

      if (!is.null(repertoires)) {
        self$schema_repertoire <- setdiff(colnames(repertoires), c(imd_schema()$repertoire, imd_schema()$n_receptors, imd_schema()$n_barcodes, imd_schema()$n_cells))
        private$.repertoire_table <- repertoires
      }
    }
  ),
  active = list(

    #' @field receptors Accessor for the dynamically-created table with receptors.
    receptors = function() {
      receptor_id_col <- imd_schema("receptor")
      barcode_col <- imd_schema("barcode")
      locus_col <- imd_schema("locus")
      features <- imd_receptor_features(self$schema_receptor)
      chains <- imd_receptor_chains(self$schema_receptor)

      if (length(chains) == 2) {
        receptor_data <- private$.annotations |>
          select(all_of(c(
            receptor_id_col,
            barcode_col,
            features,
            locus_col
          )))

        locus_1 <- chains[1]
        locus_2 <- chains[2]

        receptor_data |>
          filter(!!rlang::sym(locus_col) == locus_1) |>
          full_join(
            receptor_data |>
              filter(!!rlang::sym(locus_col) == locus_2),
            by = c(receptor_id_col, barcode_col)
          )
      } else {
        private$.annotations |>
          select({{ receptor_id_col }}, all_of(features)) |>
          distinct(!!rlang::sym(receptor_id_col), .keep_all = TRUE)
      }
    },

    #' @field annotations Accessor for the annotation-level table (`.annotations`).
    annotations = function() {
      private$.annotations
    },

    #' @field repertoires Get a vector of repertoire names after data aggregation with [agg_repertoires()]
    repertoires = function() {
      # TODO: cache repertoire table to memory if not very big?
      if (!is.null(private$.repertoire_table)) {
        private$.repertoire_table |>
          collect() |>
          arrange_at(vars(1))
      } else {
        NULL
      }
    }
  )
)
