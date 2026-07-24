#' @title ImmunData: A Unified Structure for Immune Receptor Repertoire Data
#'
#' @description
#' `ImmunData` is an abstract R6 class for managing and transforming immune receptor repertoire data.
#' It supports flexible backends (e.g., Arrow, DuckDB, dbplyr) and lazy evaluation,
#' and provides tools for filtering, aggregation, and receptor-to-repertoire mapping.
#'
#' @seealso [read_repertoires()], [read_immundata()]
#'
#' @concept core_immundata
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
    .repertoire_table = NULL,

    # .strata_table A duckplyr table with one row per stratum.
    .strata_table = NULL,

    # .provenance Internal snapshot/provenance metadata used by IO helpers.
    .provenance = NULL
  ),
  public = list(
    #' @field schema_receptor A named list describing how to interpret receptor-level data.
    #'   This includes the fields used for aggregation (e.g., `CDR3`, `V_gene`, `J_gene`),
    #'   and optionally unique identifiers for each receptor row. Used to ensure consistency
    #'   across processing steps.
    schema_receptor = NULL,

    #' @field schema_repertoire A character vector defining how barcodes or annotations should be
    #'   grouped into repertoires. This may include sample-level metadata (e.g., `sample_id`,
    #'   `donor_id`) used to define unique repertoires.
    schema_repertoire = NULL,

    #' @field schema_strata A character vector naming repertoire-level columns
    #'   used to group repertoires into strata.
    schema_strata = NULL,

    #' @description Creates a new `ImmunData` object.
    #' This constructor expects receptor-level and barcode-level data,
    #' along with a receptor schema defining aggregation and identity fields.
    #'
    #' @param schema A character vector specifying the receptor schema (e.g., aggregate fields, ID columns).
    #' @param annotations A cell/barcode-level dataset mapping barcodes to receptor rows.
    #' @param repertoires A repertoire table, created inside the body of [agg_repertoires].
    #' @param provenance Internal provenance metadata for snapshot lineage.
    #' @param strata An optional strata table containing the strata ID, name, and
    #'   columns defining the strata schema.
    initialize = function(schema,
                          annotations,
                          repertoires = NULL,
                          provenance = NULL,
                          strata = NULL) {
      checkmate::assert_data_frame(annotations)
      checkmate::assert_data_frame(repertoires, null.ok = TRUE)
      checkmate::assert_data_frame(strata, null.ok = TRUE)
      checkmate::assert_list(provenance, null.ok = TRUE)

      if (checkmate::test_character(schema)) {
        schema <- make_receptor_schema(features = schema, chains = NULL)
      }

      private$.annotations <- annotations
      self$schema_receptor <- schema
      private$.provenance <- provenance

      if (!is.null(repertoires)) {
        self$schema_repertoire <- setdiff(
          colnames(repertoires),
          c(
            imd_schema()$repertoire,
            imd_schema()$strata,
            imd_schema()$strata_name,
            imd_schema()$n_receptors,
            imd_schema()$n_barcodes,
            imd_schema()$n_cells
          )
        )
        private$.repertoire_table <- repertoires
      }

      if (!is.null(strata)) {
        if (is.null(repertoires)) {
          cli::cli_abort("A {.field strata} table requires a non-null {.field repertoires} table.")
        }

        internal_strata_columns <- c(
          imd_schema("strata"),
          imd_schema("strata_name")
        )
        missing_internal_columns <- setdiff(internal_strata_columns, colnames(strata))
        if (length(missing_internal_columns) > 0) {
          cli::cli_abort(
            "Strata table is missing required column(s): [{missing_internal_columns}]."
          )
        }

        self$schema_strata <- setdiff(colnames(strata), internal_strata_columns)
        if (length(self$schema_strata) == 0) {
          cli::cli_abort("Strata table must contain at least one strata schema column.")
        }

        missing_repertoire_schema <- setdiff(
          self$schema_strata,
          self$schema_repertoire
        )
        if (length(missing_repertoire_schema) > 0) {
          cli::cli_abort(
            "Strata schema column(s) [{missing_repertoire_schema}] are not part of the inferred repertoire schema."
          )
        }

        missing_repertoire_columns <- setdiff(colnames(strata), colnames(repertoires))
        if (length(missing_repertoire_columns) > 0) {
          cli::cli_abort(
            "Strata column(s) [{missing_repertoire_columns}] are missing from {.field repertoires}."
          )
        }

        private$.strata_table <- strata
      }
    }
  ),
  active = list(
    #' @field receptors Accessor for the dynamically-created table with receptors.
    receptors = function() {
      receptor_id_col <- imd_schema("receptor")
      locus_col <- imd_schema("locus")
      features <- imd_receptor_features(self$schema_receptor)
      chains <- imd_receptor_chains(self$schema_receptor)

      if (length(chains) == 2) {
        receptor_data <- private$.annotations |>
          select(all_of(c(
            receptor_id_col,
            features,
            locus_col
          ))) |>
          distinct()

        if (!grepl("\\|", chains[2])) {
          locus_1 <- chains[1]
          locus_2 <- chains[2]

          receptor_data |>
            filter(!!rlang::sym(locus_col) == locus_1) |>
            full_join(
              receptor_data |>
                filter(!!rlang::sym(locus_col) == locus_2),
              by = receptor_id_col
            )
        } else {
          relaxed_chain_alternatives <- trimws(unlist(strsplit(chains[2], "\\|")))
          locus_1 <- chains[1]
          locus_2 <- relaxed_chain_alternatives[1]
          locus_3 <- relaxed_chain_alternatives[2]

          receptor_data |>
            filter(!!rlang::sym(locus_col) == locus_1) |>
            full_join(
              receptor_data |>
                filter(!!rlang::sym(locus_col) %in% c(locus_2, locus_3)),
              by = receptor_id_col
            )
        }
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

    #' @field repertoires Get a table of repertoires and their basic statistics.
    repertoires = function() {
      # TODO: cache repertoire table to memory if not very big?
      if (!is.null(private$.repertoire_table)) {
        repertoire_table <- private$.repertoire_table |>
          collect()

        if (imd_schema("repertoire") %in% colnames(repertoire_table)) {
          repertoire_table <- repertoire_table |>
            arrange(.data[[imd_schema("repertoire")]])
        }

        repertoire_table
      } else {
        NULL
      }
    },

    #' @field strata Get one row per stratum with its schema values and label.
    strata = function() {
      if (!is.null(private$.strata_table)) {
        strata_table <- private$.strata_table |>
          collect()

        if (imd_schema("strata") %in% colnames(strata_table)) {
          strata_table <- strata_table |>
            arrange(.data[[imd_schema("strata")]])
        }

        strata_table
      } else {
        NULL
      }
    },

    #' @field provenance Read-only accessor for snapshot provenance metadata.
    provenance = function(value) {
      if (missing(value)) {
        return(imd_get_provenance(self))
      }

      cli::cli_abort("`provenance` is read-only and cannot be assigned directly.")
    }
  )
)
