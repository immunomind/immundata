#' @title ImmunData: A data structure for storing adaptive immune receptor repertoire data
#'
#' @description
#' `ImmunData` stores adaptive immune receptor repertoire (AIRR) data and the rules
#' used to turn observed sequences or cells into data units for analysis. Think AnnData
#' or SeuratObject, but for immune repertoires.
#'
#' You work with an `ImmunData` object after importing bulk or single-cell AIRR-seq
#' data. The major idea behind `ImmunData` is that because sequencing provides only
#' information about sequences and, for single-cell data, cell
#' barcodes, the your responsibility is to determine, which sequences you want to treat as the
#' same receptor, repertoire, or stratum (group of repertoires). You define these analysis units with
#' schemas. A schema is a stored set of column names and chain-selection rules
#' that tells `ImmunData` how to group observations. Those definitions are kept
#' inside `ImmunData` to ensure that downstream functions count, filter, and compare
#' the same units consistently. Repertoire and strata schemas can be changed later to re-aggregate
#' repertoires differently, e.g., merge receptors from different clusters into
#' per-patient clusters. Receptor schema is fixed once and for all, so if you want
#' to work with a different receptor definition, e.g., use "CDR3aa + V gene" instead of
#' just "CDR3aa" as a definiton for a unique receptor, you will need to create
#' a separate `ImmunData` object.
#'
#' `ImmunData` is immutable, meaning that functions that transform an `ImmunData`
#' object return a new object, and the original object is not changed. Due to multiple
#' optimisations on the backend, it does not mean that you re-create the whole
#' dataset each time you run a, let's stay, a filter. However, it does affect analysis workflow
#' significantly. You can read about it more on the website and in tutorials.
#'
#' @section From observed data to analysis units:
#'
#' `ImmunData` connects observed records to user-defined analysis units:
#'
#' * A **chain observation** is an observed receptor-chain sequence, such as a
#'   TRA, TRB, or IGH sequence. Chain observations form the main table.
#' * A **barcode** is an observed identifier for a cell in single-cell data. It
#'   links chains found in the same cell.
#' * A **receptor** is a virtual analysis unit that you define. For example, you
#'   may define it by CDR3 sequence alone, by CDR3 and V gene, or as a paired
#'   TRA-TRB receptor. The receptor schema records which chain features and loci
#'   must match for observations to receive the same receptor identifier.
#' * A **repertoire** is a virtual collection of receptors that you define from
#'   annotation columns. For example, one repertoire may contain all receptors
#'   from one sample, or from one donor at one time point.
#' * A **stratum** is a virtual collection of repertoires for a comparison. For
#'   example, one stratum may contain all repertoires from one treatment arm.
#'
#' These definitions do not change the observed sequences. They determine how
#' observations are grouped and counted during analysis. The resulting
#' hierarchy is `chain observations and barcodes -> receptors -> repertoires ->
#' strata`.
#'
#' @section Inspect and transform an object:
#'
#' Print an object for a compact overview. Use `$receptors` for the receptor
#' table, `$repertoires` for one summary row per repertoire, and `$strata` for
#' one row per stratum. Most analysis functions accept the complete
#' `ImmunData` object directly.
#'
#' Common transformations include:
#'
#' * [filter_immundata()] to keep selected chains, cells, or receptors;
#' * [mutate_immundata()] to calculate annotation columns;
#' * [annotate()] to add external biological information;
#' * [agg_repertoires()] to define repertoires; and
#' * [agg_strata()] to group repertoires into strata.
#'
#' @section Create an object:
#'
#' Create an `ImmunData` object with [read_repertoires()], or reopen a saved
#' object with [read_immundata()]. Do not call the `$new()` constructor in
#' analysis code. Direct construction is reserved for package developers.
#'
#' @section Lazy data and storage:
#'
#' The chain-level table uses duckplyr and can remain on disk. Filtering,
#' mutation, and aggregation stay lazy when possible, so large datasets do not
#' need to be loaded fully into R memory. Downstream analysis functions in the
#' `immunarch` package are designed to accept lazy `ImmunData` objects. Pass the
#' object directly; you usually do not need to call [dplyr::collect()]. Collect
#' data only when another function explicitly requires an in-memory data frame
#' or when you want to inspect a small table in R.
#'
#' Objects created by [read_repertoires()] are backed by files in their output
#' folder. Keep that folder while you use the object. Use [write_immundata()] to
#' save a transformed object and [read_immundata()] to reopen it.
#'
#' @seealso [read_repertoires()], [read_immundata()], [write_immundata()],
#'   [agg_repertoires()], [agg_strata()], [filter_immundata()],
#'   [mutate_immundata()], [annotate()]
#'
#' @examplesIf identical(Sys.getenv("IMD_RUN_EXAMPLES"), "true")
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Load the small dataset included with immundata, then define one repertoire
#' # for each treatment-response group.
#' idata <- get_test_idata() |>
#'   agg_repertoires(schema = "Response")
#'
#' idata$repertoires |>
#'   select(Response, n_barcodes, n_receptors) |>
#'   arrange(Response)
#' # Expected result:
#' #   Response n_barcodes n_receptors
#' #   FR              955         871
#' #   PR              947         867
#'
#' # Under the current receptor definition, the full-response (FR) repertoire
#' # contains 955 chain observations grouped into 871 receptor units.
#'
#' # Keep only the full-response repertoire. filter() returns a new object.
#' fr_only <- idata |>
#'   filter(Response == "FR")
#'
#' tibble(
#'   original_repertoires = nrow(idata$repertoires),
#'   filtered_repertoires = nrow(fr_only$repertoires)
#' )
#' # Expected result:
#' #   original_repertoires filtered_repertoires
#' #                      2                    1
#' # The original object still contains both repertoires.
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
    #' @field schema_receptor A named list defining the virtual receptor unit.
    #'   The `features` element names the chain columns used to group
    #'   observations, such as CDR3 sequence and V gene. The `chains` element
    #'   selects one chain or a paired set of chains.
    schema_receptor = NULL,

    #' @field schema_repertoire A character vector naming annotation columns
    #'   whose unique combinations define one repertoire, such as `sample_id`
    #'   or `c("donor_id", "timepoint")`. It is `NULL` when repertoires have
    #'   not been defined.
    schema_repertoire = NULL,

    #' @field schema_strata A character vector naming repertoire-level columns
    #'   whose unique combinations define one stratum, such as `treatment`. It
    #'   is `NULL` when strata have not been defined.
    schema_strata = NULL,

    #' @description Low-level constructor for package developers. Analysis code
    #' must create an `ImmunData` object with [read_repertoires()] or reopen one
    #' with [read_immundata()].
    #'
    #' @param schema A character vector or named list. A character vector names
    #'   the features used to define a chain-agnostic receptor. A named list is
    #'   created by [make_receptor_schema()] and can also select receptor chains.
    #' @param annotations A duckplyr table. It contains retained chain
    #'   observations, receptor identifiers, and biological annotations.
    #' @param repertoires A data frame or `NULL`. It contains one row per
    #'   repertoire and its summary statistics and is usually created by
    #'   [agg_repertoires()].
    #' @param provenance A list or `NULL`. It contains internal storage and
    #'   snapshot history.
    #' @param strata A data frame or `NULL`. It contains one row per stratum,
    #'   its label, and the repertoire-level columns that define it.
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
      private$.provenance <- if (is.null(provenance)) {
        NULL
      } else {
        normalize_provenance(provenance)
      }

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
    #' @field receptors A derived duckplyr table of distinct receptors. For a
    #'   paired receptor, the selected chain features are shown side by side.
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

    #' @field annotations The lazy duckplyr table of retained chain
    #'   observations and their biological annotations. For most tasks, pass the
    #'   complete `ImmunData` object to a transformation function or use
    #'   `collect(idata)` to inspect this table in memory.
    annotations = function() {
      private$.annotations
    },

    #' @field repertoires A small table with one row per repertoire, the columns
    #'   that define it, and summary statistics such as `n_barcodes` and
    #'   `n_receptors`. It is `NULL` when repertoires have not been defined.
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

    #' @field strata A small table with one row per stratum, its label, and the
    #'   repertoire-level columns that define it. It is `NULL` when strata have
    #'   not been defined.
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

    #' @field provenance Read-only named list describing the snapshot origin
    #'   and storage context carried by this object. Retrieve the complete list with
    #'   `idata$provenance`, or one field with, for example,
    #'   `idata$provenance$current_path`. The fields are:
    #'
    #'   * `home_path`: project home used for managed snapshots and artifacts.
    #'     The original ingestion snapshot is stored directly in this folder;
    #'     it is `NULL` for an object with no persisted home.
    #'   * `current_path`: exact folder of the most recently loaded or written
    #'     snapshot. Transformations preserve this source path until the
    #'     transformed object is written as another snapshot; it is `NULL` for
    #'     an object that has never been loaded from or written to disk.
    #'   * `snapshot_root`: derived managed-snapshot root,
    #'     `home_path/snapshots`, or `NULL` when `home_path` is `NULL`.
    #'   * `artifacts_root`: derived project-level root for optional external
    #'     tool outputs, `home_path/artifacts`, or `NULL` when `home_path` is
    #'     `NULL`.
    #'   * `artifacts_path`: derived namespace for artifacts associated with the
    #'     most recently loaded or written snapshot. It is
    #'     `artifacts_root/root` for the original
    #'     ingestion, `artifacts_root/<tag>/vNNN` for a managed snapshot, and
    #'     `artifacts_root/by-id/<snapshot_id>` for a detached explicit snapshot.
    #'     External tools can append `<tool>/<run>` and create that directory;
    #'     artifact contents are not part of `ImmunData`. Write a transformed
    #'     object as a new snapshot before storing artifacts that should be
    #'     associated with the transformed data.
    #'   * `snapshot_id`: unique identifier generated when the snapshot is
    #'     written; `NULL` for an in-memory object that has never been written.
    #'   * `lineage`: ordered list of ingestion and snapshot events leading to
    #'     the current snapshot.
    #'
    #'   The accessor is read-only; assigning to `idata$provenance` is an error.
    provenance = function(value) {
      if (missing(value)) {
        return(get_provenance(self))
      }

      cli::cli_abort("`provenance` is read-only and cannot be assigned directly.")
    }
  )
)

clone_with_annotations <- function(idata, annotations) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_data_frame(annotations)

  ImmunData$new(
    schema = idata$schema_receptor,
    annotations = annotations,
    repertoires = idata$repertoires,
    strata = idata$strata,
    provenance = get_provenance(idata)
  )
}
