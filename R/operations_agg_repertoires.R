#' @title Define biological repertoires and calculate receptor abundance
#'
#' @description
#' Use `agg_repertoires()` to define which receptor observations belong to the
#' same biological repertoire and calculate receptor abundance within each
#' repertoire.
#'
#' Use this function after importing data without repertoire definitions, or
#' when you want to redefine repertoires using sample information. One
#' repertoire usually represents one biological sample. It can also represent
#' one sample and time-point combination. The columns in `schema` define these
#' groups.
#'
#' The unit being defined is the repertoire. The function does not remove chain
#' rows or redefine cells or receptors. It returns a new [ImmunData] object. The
#' original object is not changed.
#'
#' @details
#' The function calculates summaries at repertoire and receptor levels while
#' keeping the original chain rows.
#'
#' @section What the function calculates:
#'
#' The returned repertoire summary contains one row for each repertoire:
#'
#' * `imd_repertoire_id`: a new integer identifier for the repertoire.
#' * `n_barcodes`: the number of observed cells for single-cell data, or the
#'   total abundance for bulk data.
#' * `n_receptors`: the number of distinct receptors in the repertoire.
#'
#' The function also adds these values to each chain row:
#'
#' * `imd_repertoire_id`: the repertoire containing the row.
#' * `imd_count`: the number of cells carrying that receptor in single-cell
#'   data, or its summed abundance in bulk data, within the repertoire.
#' * `imd_proportion`: the receptor's fraction of the repertoire, calculated as
#'   `imd_count / n_barcodes`.
#' * `n_repertoires`: the number of repertoires in which the receptor occurs.
#'
#' Values calculated for a receptor are repeated on all chain rows belonging to
#' that receptor in the same repertoire.
#'
#' Calling `agg_repertoires()` again replaces previous repertoire definitions,
#' receptor counts, proportions, and related strata summaries.
#'
#' @section Backend and storage:
#'
#' Large-table calculations run on the duckplyr annotation table. The annotation
#' data remain lazy when the input is lazy. The small repertoire summary is
#' collected and stored in the returned object.
#'
#' Aggregation can be expensive for a large dataset. After checking the result,
#' consider saving it so later analyses do not repeat the calculation. Use
#' `write_immundata(idata, tag = "by-sample")` to create a managed snapshot in
#' the object's project home. Managed snapshots are versioned, so another write
#' with the same tag creates a new version and keeps the earlier version.
#'
#' Use `write_immundata(idata, output_folder = "path/to/result")` when you need a
#' standalone saved state in a specific folder, for example to share it or to
#' choose a new storage location. Unlike a managed snapshot, writing to an
#' existing explicit folder replaces the ImmunData files in that folder. Both
#' forms materialize pending duckplyr calculations and return a disk-backed
#' object that can be reopened with [read_immundata()].
#'
#' @param idata An [ImmunData] object containing receptor observations and the
#'   columns named in `schema`. This is usually created by [read_repertoires()]
#'   or [read_immundata()].
#' @param schema A non-empty character vector. One or more column names that
#'   together define a repertoire. For example, `"Sample"` creates one
#'   repertoire per sample, and
#'   `c("Sample", "TimePoint")` creates one repertoire per sample and time-point
#'   combination. The default is `"repertoire_id"`; this column must exist if
#'   the default is used.
#' @param verbose A logical value. Accepted for consistency with other
#'   aggregation functions. It currently does not change the output. Defaults to
#'   `getOption("immundata.verbose", TRUE)`.
#'
#' @return A new [ImmunData] object with repertoire definitions and abundance
#'   statistics. Its repertoire summary contains the `schema` columns,
#'   `imd_repertoire_id`, `n_barcodes`, and `n_receptors`. Its chain rows also
#'   contain `imd_repertoire_id`, `imd_count`, `imd_proportion`, and
#'   `n_repertoires`.
#'
#' @seealso [read_repertoires()], [agg_strata()], [write_immundata()], [ImmunData]
#'
#' @concept aggregation
#' @export
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Create a small bulk T-cell receptor dataset from two biological samples
#' bulk_data <- tibble(
#'   Sample = c("Tumor", "Tumor", "Blood", "Blood"),
#'   cdr3_aa = c("CASSA", "CASSB", "CASSA", "CASSC"),
#'   v_call = c("TRBV1", "TRBV2", "TRBV1", "TRBV3"),
#'   abundance = c(20L, 5L, 4L, 6L)
#' )
#'
#' bulk_file <- tempfile(fileext = ".tsv")
#' readr::write_tsv(bulk_data, bulk_file)
#'
#' # Import receptors without defining repertoires
#' idata <- read_repertoires(
#'   path = bulk_file,
#'   schema = c("cdr3_aa", "v_call"),
#'   count_col = "abundance",
#'   repertoire_schema = NULL,
#'   output_folder = tempfile("immundata-example-")
#' )
#'
#' # Define one repertoire for each biological sample
#' sample_repertoires <- idata |>
#'   agg_repertoires(schema = "Sample")
#'
#' sample_repertoires$repertoires |>
#'   select(Sample, n_barcodes, n_receptors) |>
#'   arrange(Sample)
#' # Expected result:
#' #   Sample n_barcodes n_receptors
#' #   Blood          10           2
#' #   Tumor          25           2
#'
#' # For example, CASSA forms 80% of the Tumor repertoire and 40% of the
#' # Blood repertoire. It occurs in two repertoires.
#'
#' # For a large dataset, save the result as a managed snapshot so this
#' # aggregation does not need to run again.
#' saved_repertoires <- write_immundata(
#'   sample_repertoires,
#'   tag = "by-sample"
#' )
agg_repertoires <- function(idata, schema = "repertoire_id",
                            verbose = getOption("immundata.verbose", TRUE)) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(schema, min.len = 1)
  checkmate::assert_flag(verbose)

  missing_cols <- setdiff(schema, colnames(idata$annotations))
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns in `annotations`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  receptor_id <- imd_schema("receptor")
  repertoire_id <- imd_schema("repertoire")
  prop_col <- imd_schema("proportion")
  imd_count_col <- imd_schema("count")
  barcode_col <- imd_schema("barcode")
  chain_count_col <- imd_schema("chain_count")
  n_receptors_col <- imd_schema("n_receptors")
  n_barcodes_col <- imd_schema("n_barcodes")
  n_repertoires_col <- imd_schema("n_repertoires")
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  # Remove columns from the previous repertoire aggregation if any
  cols_to_drop <- c(repertoire_id, strata_col, strata_name_col, imd_count_col, prop_col, n_receptors_col, n_barcodes_col, n_repertoires_col)

  new_annotations <- idata$annotations |>
    select(-any_of(cols_to_drop))

  single_chain_annotations <- new_annotations |>
    # Deduplicate receptor/barcode rows without distinct(.keep_all = TRUE),
    # which is unstable on duckdb 1.5.x due to the optimizer.
    # https://github.com/duckdb/duckdb/issues/21348
    summarise(
      .by = all_of(c(schema, receptor_id, barcode_col)),
      {{ chain_count_col }} := dplyr::first(!!rlang::sym(chain_count_col))
    )

  #
  # proportions
  #
  receptor_cells <- single_chain_annotations |>
    summarise(
      .by = all_of(c(schema, receptor_id)),
      {{ imd_count_col }} := sum(!!rlang::sym(chain_count_col))
    )

  repertoires_table <- receptor_cells |>
    summarise(
      .by = all_of(schema),
      !!n_barcodes_col := sum(!!rlang::sym(imd_count_col)),
      !!n_receptors_col := n()
    ) |>
    arrange(!!!rlang::syms(schema)) |>
    mutate(
      {{ repertoire_id }} := row_number()
    ) |>
    relocate({{ repertoire_id }}) |>
    collect()

  receptor_props <- receptor_cells |>
    left_join(repertoires_table, by = schema, na_matches = "na") |>
    mutate(!!prop_col := !!rlang::sym(imd_count_col) / !!rlang::sym(n_barcodes_col)) |>
    select(-all_of(c(n_barcodes_col, n_receptors_col)))

  new_annotations <- new_annotations |>
    left_join(
      receptor_props,
      by = c(schema, receptor_id),
      na_matches = "na"
    )

  #
  # n_repertoires
  #
  repertoire_counts <- receptor_cells |>
    summarise(.by = all_of(receptor_id), n_repertoires = n())

  new_annotations <- new_annotations |>
    left_join(repertoire_counts, by = receptor_id, na_matches = "na")

  ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    repertoires = repertoires_table,
    provenance = get_provenance(idata)
  )
}
