#' @title Aggregate AIRR data into repertoires
#'
#' @description
#' Groups the annotation table of an `ImmunData` object by user-specified
#' columns to define distinct *repertoires* (e.g., based on sample, donor,
#' time point). It then calculates summary statistics both per-repertoire and
#' per-receptor within each repertoire.
#'
#' Calculated **per repertoire**:
#' * `n_barcodes`: Total number of unique cells/barcodes within the repertoire
#'     (sum of `imd_chain_count`, effectively summing unique cells if input was SC,
#'     or total counts if input was bulk).
#' * `n_receptors`: Number of unique receptors (`imd_receptor_id`) found within
#'     the repertoire.
#'
#' Calculated **per annotation row** (receptor within repertoire context):
#' * `imd_count`: Total count of a specific receptor (`imd_receptor_id`) within
#'     the specific repertoire it belongs to in that row (sum of relevant
#'     `imd_chain_count`).
#' * `imd_proportion`: The proportion of the repertoire's total `n_barcodes`
#'     accounted for by that specific receptor (`imd_count / n_barcodes`).
#' * `n_repertoires`: The total number of distinct repertoires (across the entire
#'     dataset) in which this specific receptor (`imd_receptor_id`) appears.
#'
#' These statistics are added to the annotation table, and a summary table is
#' stored in the `$repertoires` slot of the returned object.
#'
#' @param idata An `ImmunData` object, typically the output of [read_repertoires()]
#'   or [read_immundata()]. Must contain the `$annotations` table with columns
#'   specified in `schema` and internal columns like `imd_receptor_id` and
#'   `imd_chain_count`.
#' @param schema Character vector. Column name(s) in `idata$annotations` that
#'   define a unique repertoire. For example, `c("SampleID")` or
#'   `c("DonorID", "TimePoint")`. Columns must exist in `idata$annotations`.
#'   Default: `"repertoire_id"` (assumes such a column exists).
#'
#' @details
#' The function operates on the `idata$annotations` table:
#' 1.  **Validation:** Checks `idata` and existence of `schema` columns. Removes
#'     any pre-existing repertoire summary columns to prevent duplication.
#' 2.  **Repertoire Definition:** Groups annotations by the `schema` columns.
#'     Calculates total counts (`n_barcodes`) per group. Assigns a unique integer
#'     `imd_repertoire_id` to each distinct repertoire group. This forms the
#'     initial `repertoires_table`.
#' 3.  **Receptor Counts & Proportion:** Calculates the sum of `imd_chain_count`
#'     for each receptor within each repertoire (`imd_count`). Calculates the
#'     proportion (`imd_proportion`) of each receptor within its repertoire.
#' 4.  **Repertoire & Receptor Stats:** Counts unique receptors per repertoire
#'     (`n_receptors`, added to `repertoires_table`). Counts the number of
#'     distinct repertoires each unique receptor appears in (`n_repertoires`).
#' 5.  **Join Results:** Joins the calculated `imd_count`, `imd_proportion`, and
#'     `n_repertoires` back to the annotation table based on repertoire columns
#'     and `imd_receptor_id`.
#' 6.  **Return New Object:** Creates and returns a *new* `ImmunData` object
#'     containing the updated `$annotations` table (with the added statistics)
#'     and the `$repertoires` slot populated with the `repertoires_table`
#'     (containing `schema` columns, `imd_repertoire_id`, `n_barcodes`, `n_receptors`).
#'
#' The original `idata` object remains unmodified. Internal column names are
#' typically managed by `immundata:::imd_schema()`.
#'
#' @return A **new** `ImmunData` object. Its `$annotations` table includes the
#'   added columns (`imd_repertoire_id`, `imd_count`, `imd_proportion`, `n_repertoires`).
#'   Its `$repertoires` slot contains the summary table linking `schema` columns
#'   to `imd_repertoire_id`, `n_barcodes`, and `n_receptors`.
#'
#' @seealso [read_repertoires()] (which can call this function), [ImmunData] class.
#'
#' @concept aggregation
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume 'idata_raw' is an ImmunData object loaded via read_repertoires
#' # but *without* providing 'repertoire_schema' initially.
#' # It has $annotations but $repertoires is likely NULL or empty.
#' # Assume idata_raw$annotations has columns "SampleID" and "TimePoint".
#'
#' # Define repertoires based on SampleID and TimePoint
#' idata_aggregated <- agg_repertoires(idata_raw, schema = c("SampleID", "TimePoint"))
#'
#' # Explore the results
#' print(idata_aggregated)
#' print(idata_aggregated$repertoires)
#' print(head(idata_aggregated$annotations)) # Note the new columns
#' }
agg_repertoires <- function(idata, schema = "repertoire_id") {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(schema, min.len = 1)

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

  repertoires_table <- single_chain_annotations |>
    summarise(
      .by = all_of(schema),
      n_barcodes = sum(!!to_sym(chain_count_col))
    ) |>
    mutate(
      {{ repertoire_id }} := row_number()
    ) |>
    relocate({{ repertoire_id }})

  #
  # proportions
  #
  receptor_cells <- single_chain_annotations |>
    summarise(
      .by = all_of(c(schema, receptor_id)),
      {{ imd_count_col }} := sum(!!rlang::sym(chain_count_col))
    )

  repertoires_table_for_join <- repertoires_table

  receptor_props <- receptor_cells |>
    left_join(repertoires_table_for_join, by = schema) |>
    mutate({{ prop_col }} := !!rlang::sym(imd_count_col) / n_barcodes) |>
    select(-n_barcodes)

  new_annotations <- new_annotations |>
    left_join(receptor_props, by = c(schema, receptor_id))

  #
  # n_repertoires & n_receptors
  #
  unique_receptors <- new_annotations |>
    distinct(!!rlang::sym(receptor_id), !!rlang::sym(repertoire_id))

  n_receptor_df <- unique_receptors |>
    summarise(.by = !!rlang::sym(repertoire_id), n_receptors = n())

  repertoires_table <- repertoires_table |> left_join(n_receptor_df, by = repertoire_id)

  repertoire_counts <- unique_receptors |>
    summarise(.by = all_of(receptor_id), n_repertoires = n())

  new_annotations <- new_annotations |> left_join(repertoire_counts, by = receptor_id)

  ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    repertoires = repertoires_table,
    provenance = imd_get_provenance(idata)
  )
}
