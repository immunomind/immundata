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

  receptor_id <- imd_schema()$receptor
  repertoire_id <- imd_schema()$repertoire
  repertoire_schema_sym <- to_sym(schema)
  prop_col <- imd_schema()$proportion
  imd_count_col <- imd_schema("count")
  chain_count_col <- imd_schema("chain_count")
  n_receptors_col <- imd_schema("n_receptors")
  n_barcodes_col <- imd_schema("n_barcodes")
  n_repertoires_col <- imd_schema("n_repertoires")

  cols_to_drop <- c(repertoire_id, imd_count_col, prop_col, n_receptors_col, n_barcodes_col, n_repertoires_col)

  new_annotations <- idata$annotations |> select(-any_of(cols_to_drop))

  repertoires_table <- new_annotations |>
    summarise(
      .by = schema,
      n_barcodes = sum(!!to_sym(chain_count_col))
    ) |>
    mutate(
      {{ repertoire_id }} := row_number()
    ) |>
    relocate({{ repertoire_id }})

  #
  # proportions
  #
  receptor_cells <- new_annotations |> summarise(
    .by = c(schema, receptor_id),
    {{ imd_count_col }} := sum(!!rlang::sym(chain_count_col))
  )

  receptor_props <- receptor_cells |>
    left_join(repertoires_table, by = schema) |>
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
    repertoires = repertoires_table
  )
}


#' @title Aggregates AIRR data into receptors
#'
#' @description
#' Processes a table of immune receptor sequences (chains or clonotypes) to
#' identify unique receptors based on a specified schema. It assigns a unique
#' identifier (`imd_receptor_id`) to each distinct receptor signature and
#' returns an annotated table linking the original sequence data to these
#' receptor IDs.
#'
#' This function is a core component used within [read_repertoires()] and handles
#' different input data structures:
#' * Simple tables (no counts, no cell IDs).
#' * Bulk sequencing data (using a count column).
#' * Single-cell data (using a barcode/cell ID column). For single-cell data,
#'     it can perform chain pairing if the schema specifies multiple chains
#'     (e.g., TRA and TRB).
#'
#' @param dataset A data frame or `duckplyr_df` containing sequence/clonotype data.
#'   Must include columns specified in `schema` and potentially `barcode_col`,
#'   `count_col`, `locus_col`, `umi_col`. Could be `idata$annotations`.
#' @param schema Defines how a unique receptor is identified. Can be:
#'   * A character vector of column names representing receptor features
#'       (e.g., `c("v_call", "j_call", "junction_aa")`).
#'   * A list created by `make_receptor_schema()`, specifying both `features`
#'       (character vector) and optionally `chains` (character vector of locus
#'       names like `"TRA"`, `"TRB"`, `"IGH"`, `"IGK"`, `"IGL"`, max length 2).
#'       Specifying `chains` triggers filtering by locus and enables pairing logic
#'       if two chains are given.
#' @param barcode_col Character(1). The name of the column containing cell
#'   identifiers (barcodes). Required for single-cell processing and chain pairing.
#'   Default: `NULL`.
#' @param count_col Character(1). The name of the column containing counts
#'   (e.g., UMI counts for bulk, clonotype frequency). Used for bulk data
#'   processing. Default: `NULL`. Cannot be specified if `barcode_col` is set.
#' @param locus_col Character(1). The name of the column specifying the chain locus
#'   (e.g., "TRA", "TRB"). Required if `schema` includes `chains` for filtering
#'   or pairing. Default: `NULL`.
#' @param umi_col Character(1). The name of the column containing UMI counts.
#'   Required for *paired-chain single-cell* data (`length(schema$chains) == 2`).
#'   Used to select the most abundant chain per locus within a cell when multiple
#'   chains of the same locus are present. Default: `NULL`.
#'
#' @details
#' The function performs the following main steps:
#' 1.  **Validation:** Checks inputs, schema validity, and existence of required columns.
#' 2.  **Schema Parsing:** Determines receptor features and target chains from `schema`.
#' 3.  **Locus Filtering:** If `schema$chains` is provided, filters the dataset
#'     to include only rows matching the specified locus/loci.
#' 4.  **Processing Logic (based on `barcode_col` and `count_col`):**
#'     * **Simple Table/Bulk (No Barcodes):** Assigns unique internal barcode/chain IDs.
#'         Identifies unique receptors based on `schema$features`. Calculates
#'         `imd_chain_count` (1 for simple table, from `count_col` for bulk).
#'     * **Single-Cell (Barcodes Provided):** Uses `barcode_col` for `imd_barcode_id`.
#'         * **Single Chain:** (`length(schema$chains) <= 1`). Identifies unique
#'             receptors based on `schema$features`. `imd_chain_count` is 1.
#'         * **Paired Chain:** (`length(schema$chains) == 2`). Requires `locus_col`
#'             and `umi_col`. Filters chains within each cell/locus group based
#'             on max `umi_col`. Creates paired receptors by joining the two
#'             specified loci for each cell based on `schema$features` from both.
#'             Assigns a unique `imd_receptor_id` to each *pair*.
#'             `imd_chain_count` is 1 (representing the chain record).
#' 5.  **Output:** Returns an annotated data frame containing original columns plus
#'     internal identifiers (`imd_receptor_id`, `imd_barcode_id`, `imd_chain_id`)
#'     and counts (`imd_chain_count`).
#'
#' Internal column names are typically managed by `immundata:::imd_schema()`.
#'
#' @return A `duckplyr_df` (or data frame) representing the annotated sequences.
#'   This table links each original sequence record (chain) to a defined receptor
#'   and includes standardized columns:
#'   * `imd_receptor_id`: Integer ID unique to each distinct receptor signature.
#'   * `imd_barcode_id`: Integer ID unique to each cell/barcode (or row if no barcode).
#'   * `imd_chain_id`: Integer ID unique to each input row (chain).
#'   * `imd_chain_count`: Integer count associated with the chain (1 for SC/simple,
#'       from `count_col` for bulk).
#'   This output is typically assigned to the `$annotations` field of an `ImmunData` object.
#'
#' @seealso [read_repertoires()], [make_receptor_schema()], [ImmunData]
#'
#' @export
agg_receptors <- function(dataset, schema, barcode_col = NULL, count_col = NULL, locus_col = NULL, umi_col = NULL) {
  checkmate::assert_data_frame(dataset)
  checkmate::check_character(barcode_col, max.len = 1, null.ok = TRUE)
  checkmate::check_character(count_col, max.len = 1, null.ok = TRUE)
  checkmate::check_character(locus_col, max.len = 1, null.ok = TRUE)
  checkmate::check_character(locus_col, max.len = 1, null.ok = TRUE)

  if (checkmate::test_character(schema, min.len = 1)) {
    schema <- make_receptor_schema(schema)
  } else if (assert_receptor_schema(schema)) {
    if (!is.null(schema$locus)) {
      if (is.null(locus_col)) {
        cli::cli_abort("Found issues with the schema. The passed schema has a `chain` to aggregate receptors by, but `'locus_col'` is NULL. Please provide `'locus_col'` or aggregate receptors without using several chains.")
      } else if (is.null(barcode_col) && length(schema$locus) == 2) {
        cli::cli_abort("Found issues with the schema. The passed schema has a `chain` to aggregate receptors by, but `'barcode_col'` is NULL. Please provide `'barcode_col'` or aggregate receptors without using several chains.")
      }
    }
  } else {
    cli::cli_abort("Found issues with the schema. Please either pass one or several column names or use function {.run immundata::check_receptor_schema()} to create a schema.")
  }

  receptor_features <- imd_receptor_features(schema)
  receptor_chains <- imd_receptor_chains(schema)

  receptor_cols_existence <- setdiff(c(receptor_features, locus_col), colnames(dataset))
  if (length(receptor_cols_existence) != 0) {
    cli::cli_abort("Not all columns in the receptor schema present in the data: [{receptor_cols_existence}]. Please double check and run again.")
  }

  # TODO:
  #   if (checkmate::test_r6(idata, "ImmunData")) {
  #     dataset <- idata$annotations
  #   } else {
  #     dataset <- idata
  #   }

  immundata_barcode_col <- imd_schema("barcode")
  immundata_receptor_id_col <- imd_schema("receptor")
  immundata_chain_id_col <- imd_schema("chain")
  immundata_count_col <- imd_schema("count")
  immundata_chain_count <- imd_schema("chain_count")

  # TODO: refactor
  if (!is.null(locus_col)) {
    if (locus_col != imd_schema("locus")) {
      cli::cli_alert_info("Renaming {locus_col} to {imd_schema('locus')}")

      locus_col <- imd_schema("locus")
      names(locus_col) <- locus_col

      dataset <- rename(locus_col)
    }
  }

  # TODO:
  # Prefilter locus
  if (is.null(receptor_chains)) {
    cli::cli_alert_info("No locus information found")
  } else if (length(receptor_chains) == 1) {
    # '==' should be faster than 'in' hence a separate use case.
    dataset <- dataset |> filter(!!rlang::sym(locus_col) == receptor_chains)
    cli::cli_alert_info("Found target locus: {receptor_chains}. The dataset will be pre-filtered to leave chains for this locus only")
  } else {
    dataset <- dataset |> filter(!!rlang::sym(locus_col) %in% receptor_chains)
    cli::cli_alert_info("Found locus pair: {receptor_chains}. The dataset will be pre-filtered to leave chains for these loci only")
  }


  #
  # 1) Case #1: simple receptor table - no barcodes, no count column
  #
  if (is.null(barcode_col) && is.null(count_col)) {
    cli::cli_alert_info("Processing data as immune repertoire tables - no counts, no barcodes, no chain pairing possible")

    dataset <- dataset |>
      mutate(
        {{ immundata_barcode_col }} := row_number(),
        {{ immundata_chain_id_col }} := !!rlang::sym(immundata_barcode_col)
      )

    receptor_data <- dataset |>
      summarise(.by = all_of(receptor_features)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- dataset |>
      left_join(receptor_data, by = receptor_features) |>
      mutate(
        {{ immundata_chain_count }} := 1,
        {{ immundata_count_col }} := 0
      )
  }

  #
  # 2) Case 2: bulk data - no barcodes, but with the count column
  #
  else if (is.null(barcode_col) && !is.null(count_col)) {
    cli::cli_alert_info("Processing data as bulk sequencing immune repertoires - with counts, no barcodes, no chain pairing possible")

    dataset <- dataset |>
      mutate(
        {{ immundata_barcode_col }} := row_number(),
        {{ immundata_chain_id_col }} := !!rlang::sym(immundata_barcode_col)
      )

    receptor_data <- dataset |>
      summarise(.by = all_of(receptor_features)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- dataset |>
      left_join(receptor_data, by = receptor_features) |>
      mutate(
        {{ immundata_chain_count }} := !!rlang::sym(count_col),
        {{ immundata_count_col }} := 0
      )
  }

  #
  # 3) Case 3: single-cell data - barcodes, no counts
  #
  else if (!is.null(barcode_col) && is.null(count_col)) {
    cli::cli_alert_info("Processing data as single-cell sequencing immune repertoires - no counts, with barcodes, chain pairing is possible")

    dataset <- dataset |>
      mutate(
        {{ immundata_barcode_col }} := !!rlang::sym(barcode_col),
        {{ immundata_chain_id_col }} := row_number()
      )

    #
    # 3.1) Case 3.1: single chain
    #
    if (length(receptor_chains) <= 1) {
      receptor_data <- dataset |>
        summarise(.by = all_of(receptor_features)) |>
        mutate(
          {{ immundata_receptor_id_col }} := row_number()
        )

      annotation_data <- dataset |>
        left_join(receptor_data, by = receptor_features) |>
        mutate({{ immundata_chain_count }} := 1, {{ immundata_count_col }} := 0)
    }

    #
    # 3.2) Case 3.2: paired chain
    #
    else if (length(receptor_chains) == 2) {
      locus_1 <- receptor_chains[1]
      locus_2 <- receptor_chains[2]

      # Step 1: filter out bad chains, i.e., find the most abundance pairs of chains per barcode per locus
      receptor_data <- dataset |>
        select(all_of(c(immundata_barcode_col, umi_col, locus_col))) |>
        mutate(
          .by = c(immundata_barcode_col, locus_col),
          temp__reads = max(!!rlang::sym(umi_col), na.rm = TRUE)
        ) |>
        filter(!!rlang::sym(umi_col) == temp__reads)
      # distinct(!!rlang::sym(immundata_barcode_col), !!rlang::sym(locus_col), .keep_all = TRUE)

      # TODO: what if temp_reads == max with several receptors?

      # Step 2: create receptors by self-join
      # TODO: optimize plz

      r1 <- receptor_data |>
        filter(!!rlang::sym(locus_col) == locus_1)
      r2 <- receptor_data |>
        filter(!!rlang::sym(locus_col) == locus_2)

      receptor_data <- r1 |>
        semi_join(
          r2,
          by = immundata_barcode_col
        ) |>
        mutate(
          {{ immundata_receptor_id_col }} := row_number()
        ) |>
        select(all_of(c(
          immundata_receptor_id_col,
          immundata_barcode_col
        )))

      annotation_data <- receptor_data |>
        left_join(
          dataset,
          by = immundata_barcode_col
        ) |>
        mutate(
          {{ immundata_chain_count }} := 1,
          {{ immundata_count_col }} := 0
        )
    }

    #
    # Case 3.3: unsupported multiple chain
    #
    else {
      cli::cli_abort("Unsupported case: more than two chains in [{receptor_chains}]")
    }
  } else {
    #
    #  4) Something weird is happening...
    #
    cli_abort("Undefined case: passed column names for both cell identifiers and receptor counts.")
  }

  annotation_data
}


#' @title Create or validate a receptor schema object
#'
#' @description
#' Helper functions for defining and validating the `schema` used by
#' [agg_receptors()] to identify unique receptors.
#'
#' `make_receptor_schema()` creates a schema list object.
#' `assert_receptor_schema()` checks if an object is a valid schema list and throws
#'   an error if not.
#' `test_receptor_schema()` checks if an object is a valid schema list or a
#'   character vector (which `agg_receptors` can also accept) and returns `TRUE`
#'   or `FALSE`.
#'
#' @param features Character vector. Column names defining the features of a
#'   single receptor chain (e.g., V gene, J gene, CDR3 sequence).
#' @param chains Optional character vector (max length 2). Locus names (e.g.,
#'   `"TRA"`, `"TRB"`) to filter by or pair. If `NULL` or length 1, only
#'   filtering occurs. If length 2, pairing logic is enabled in [agg_receptors()].
#'   Default: `NULL`.
#' @param schema An object to test or assert as a valid schema. Can be a list
#'   created by `make_receptor_schema` or a character vector (for `test_receptor_schema`).
#'
#' @return
#' `make_receptor_schema` returns a list with elements `features` and `chains`.
#' `assert_receptor_schema` returns `TRUE` invisibly if valid, or stops execution.
#' `test_receptor_schema` returns `TRUE` or `FALSE`.
#'
#' @rdname make_receptor_schema
#' @export
make_receptor_schema <- function(features, chains = NULL) {
  checkmate::check_character(features, min.len = 1)
  checkmate::check_character(chains, max.len = 2, null.ok = TRUE)

  list(features = features, chains = chains)
}


#' @rdname make_receptor_schema
#' @export
assert_receptor_schema <- function(schema) {
  # TODO: globals.R with schema list

  checkmate::assert(
    checkmate::test_character(schema, min.len = 1),
    checkmate::test_list(schema, len = 2, null.ok = FALSE) &&
      checkmate::test_names(names(schema), must.include = c("features", "chains"))
  )
}


#' @rdname make_receptor_schema
#' @export
test_receptor_schema <- function(schema) {
  checkmate::test_character(schema, min.len = 1) || (
    checkmate::test_list(schema, len = 2, null.ok = FALSE) &&
      checkmate::test_subset(names(schema), c("features", "chains"), empty.ok = FALSE)
  )
}
