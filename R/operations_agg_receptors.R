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
#' @param dataset A `duckplyr_df` containing AIRR data.
#'   Must include columns specified in `schema` and potentially `barcode_col`,
#'   `count_col`, `locus_col`, `umi_col`. Expected `idata$annotations`,
#'   support for `ImmunData` will probably be added later.
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
#'   Required for single-cell data (`barcode_col` is set). Used to select the
#'   most abundant chain within each barcode and, for paired schemas, within
#'   each barcode/locus group when multiple chains are present. Default: `NULL`.
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
#'             receptors based on `schema$features`. Uses `umi_col` to keep one
#'             chain per barcode when needed. `imd_chain_count` is 1.
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
#' @concept aggregation
#' @export
agg_receptors <- function(dataset, schema, barcode_col = NULL, count_col = NULL, locus_col = NULL, umi_col = NULL) {
  checkmate::assert_data_frame(dataset)
  checkmate::check_character(barcode_col, max.len = 1, null.ok = TRUE)
  checkmate::check_character(count_col, max.len = 1, null.ok = TRUE)
  checkmate::check_character(locus_col, max.len = 1, null.ok = TRUE)
  checkmate::check_character(umi_col, max.len = 1, null.ok = TRUE)

  if (!is.null(barcode_col) && !is.null(count_col)) {
    cli::cli_abort("Please pass either {.arg barcode_col} (single-cell mode) or {.arg count_col} (bulk mode), not both.")
  }

  if (checkmate::test_character(schema, min.len = 1)) {
    schema <- make_receptor_schema(schema)
  } else if (assert_receptor_schema(schema)) {
    if (!is.null(schema$chains)) {
      if (is.null(locus_col)) {
        cli::cli_abort("Found issues with the schema. The passed schema has a `chain` to aggregate receptors by, but `'locus_col'` is NULL. Please provide `'locus_col'` or aggregate receptors without using several chains.")
      } else if (is.null(barcode_col) && length(schema$chains) == 2) {
        cli::cli_abort("Found issues with the schema. The passed schema has a `chain` to aggregate receptors by, but `'barcode_col'` is NULL. Please provide `'barcode_col'` or aggregate receptors without using several chains.")
      }
    }
  } else {
    cli::cli_abort("Found issues with the schema. Please either pass one or several column names or use function {.run immundata::check_receptor_schema()} to create a schema.")
  }

  receptor_features <- imd_receptor_features(schema)
  receptor_chains <- imd_receptor_chains(schema)

  if (!is.null(barcode_col) && is.null(umi_col)) {
    cli::cli_abort("Single-cell mode requires {.arg umi_col}. Please provide the column with per-chain UMI/reads to resolve chain multiplicity within each barcode.")
  }

  is_relaxed_pairing <- FALSE
  relaxed_chain_alternatives <- NULL
  parsed_chains <- receptor_chains

  if (!is.null(receptor_chains)) {
    if (length(receptor_chains) == 2) {
      if (grepl("\\|", receptor_chains[2])) {
        is_relaxed_pairing <- TRUE
        relaxed_chain_alternatives <- trimws(unlist(strsplit(receptor_chains[2], "\\|")))
        parsed_chains <- c(receptor_chains[1], relaxed_chain_alternatives)
      }
    }
  }

  receptor_cols_existence <- setdiff(receptor_features, colnames(dataset))
  if (length(receptor_cols_existence) != 0) {
    cli::cli_abort("Missing receptor feature column(s) required by {.arg schema}: [{receptor_cols_existence}].")
  }

  required_arg_cols <- c(
    if (!is.null(locus_col)) stats::setNames(locus_col, "locus_col"),
    if (!is.null(barcode_col)) stats::setNames(barcode_col, "barcode_col"),
    if (!is.null(count_col)) stats::setNames(count_col, "count_col"),
    if (!is.null(umi_col)) stats::setNames(umi_col, "umi_col")
  )

  missing_arg_cols <- setdiff(unname(required_arg_cols), colnames(dataset))
  if (length(missing_arg_cols) != 0) {
    missing_args <- names(required_arg_cols)[match(missing_arg_cols, required_arg_cols)]
    cli::cli_abort(
      "Missing column(s) referenced by arguments: [{missing_arg_cols}] (from [{missing_args}])."
    )
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
  immundata_chain_count_col <- imd_schema("chain_count")

  # TODO: refactor
  if (!is.null(locus_col)) {
    if (locus_col != imd_schema("locus")) {
      cli::cli_alert_info("Renaming {locus_col} to {imd_schema('locus')}")

      locus_col <- imd_schema("locus")
      names(locus_col) <- locus_col

      dataset <- rename(locus_col)
    }
  }

  # Prefilter locus
  if (is.null(receptor_chains)) {
    cli::cli_alert_info("No locus information found")
  } else if (length(parsed_chains) == 1) {
    dataset <- dataset |> filter(!!rlang::sym(locus_col) == parsed_chains)
    cli::cli_alert_info("Found target locus: {parsed_chains}. The dataset will be pre-filtered to leave chains for this locus only")
  } else {
    dataset <- dataset |> filter(!!rlang::sym(locus_col) %in% parsed_chains)
    if (is_relaxed_pairing) {
      cli::cli_alert_info("Found relaxed locus pair: {receptor_chains[1]} + ({receptor_chains[2]}). The dataset will be pre-filtered to leave chains for these loci only")
    } else {
      cli::cli_alert_info("Found locus pair: {receptor_chains}. The dataset will be pre-filtered to leave chains for these loci only")
    }
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
        {{ immundata_chain_count_col }} := 1,
        {{ immundata_count_col }} := 0
      )
  }

  #
  # 2) Case #2: bulk data - no barcodes, but with the count column
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
        {{ immundata_chain_count_col }} := !!rlang::sym(count_col),
        {{ immundata_count_col }} := 0
      )
  }

  #
  # 3) Case #3: single-cell data - barcodes, no counts
  #
  else if (!is.null(barcode_col) && is.null(count_col)) {
    cli::cli_alert_info("Processing data as single-cell sequencing immune repertoires - no counts, with barcodes, chain pairing is possible")

    dataset <- dataset |>
      mutate(
        {{ immundata_barcode_col }} := !!rlang::sym(barcode_col),
        {{ immundata_chain_id_col }} := row_number()
      )

    #
    # 3.1) Case #3.1: single chain
    #
    if (length(receptor_chains) <= 1) {
      # We still need to filter out receptors from barcodes
      # with more than one receptor

      filtered_chains <- dataset |>
        select(all_of(c(
          immundata_chain_id_col,
          immundata_barcode_col,
          umi_col
        ))) |>
        mutate(
          .by = all_of(immundata_barcode_col),
          temp__reads = max(!!rlang::sym(umi_col), na.rm = TRUE)
        ) |>
        filter(!!rlang::sym(umi_col) == temp__reads) |>
        distinct(!!rlang::sym(immundata_barcode_col), .keep_all = TRUE) |>
        select(all_of(c(immundata_barcode_col, immundata_chain_id_col)))

      dataset <- dataset |>
        semi_join(filtered_chains, by = immundata_chain_id_col)

      receptor_data <- dataset |>
        summarise(.by = all_of(receptor_features)) |>
        mutate(
          {{ immundata_receptor_id_col }} := row_number()
        )

      annotation_data <- dataset |>
        left_join(receptor_data, by = receptor_features) |>
        mutate({{ immundata_chain_count_col }} := 1, {{ immundata_count_col }} := 0)
    }

    #
    # 3.2) Case #3.2: paired chain
    #
    else if (length(receptor_chains) == 2) {
      paired_receptor_features <- do.call(paste0, expand.grid(c(receptor_features, locus_col), c(".x", ".y")))

      locus_1 <- parsed_chains[1]
      locus_2 <- parsed_chains[2]

      if (is_relaxed_pairing) {
        locus_3 <- parsed_chains[3]
      }

      # Step 1: find the target chains:
      # - find the most abundant pairs of chains per barcode per locus
      filtered_chains <- dataset |>
        select(all_of(c(
          immundata_chain_id_col,
          immundata_barcode_col,
          umi_col,
          locus_col
        ))) |>
        mutate(
          .by = all_of(c(immundata_barcode_col, locus_col)),
          temp__reads = max(!!rlang::sym(umi_col), na.rm = TRUE)
        ) |>
        filter(!!rlang::sym(umi_col) == temp__reads) |>
        # If there are ties, keep the first one
        distinct(!!rlang::sym(immundata_barcode_col), !!rlang::sym(locus_col), .keep_all = TRUE) |>
        select(all_of(c(immundata_barcode_col, locus_col, immundata_chain_id_col)))

      if (!is_relaxed_pairing) {
        # - find barcodes with both loci
        valid_barcodes <- filtered_chains |>
          summarise(
            .by = all_of(immundata_barcode_col),
            n = n()
          ) |>
          filter(n == 2)
      } else {
        # - find barcodes with one main locus and only one of the alternative loci
        valid_barcodes <- filtered_chains |>
          summarise(
            .by = all_of(immundata_barcode_col),
            has_l1 = any(!!rlang::sym(locus_col) == locus_1),
            has_l2 = any(!!rlang::sym(locus_col) == locus_2),
            has_l3 = any(!!rlang::sym(locus_col) == locus_3),
          ) |>
          filter(.data$has_l1, (.data$has_l2 & !.data$has_l3) | (!.data$has_l2 & .data$has_l3))
      }

      # - get back to chains to select only those which are paired
      filtered_chains <- filtered_chains |>
        semi_join(valid_barcodes,
          by = immundata_barcode_col
        )

      # Looks like back-and-forth, but I'm not sure how to make it better, tbh
      # Alternative: filter out bad barcodes first, but it would require n_distinct
      # as a first step, so pretty much the same as currently.
      # TODO: benchmark this

      # Step 2: create receptors and their identifiers by self-join

      annotated_filtered_chains <- dataset |>
        select(all_of(c(receptor_features, locus_col, immundata_chain_id_col, immundata_barcode_col))) |>
        semi_join(filtered_chains, by = immundata_chain_id_col)

      r1 <- annotated_filtered_chains |>
        filter(!!rlang::sym(locus_col) == locus_1)

      if (!is_relaxed_pairing) {
        r2 <- annotated_filtered_chains |>
          filter(!!rlang::sym(locus_col) == locus_2)
      } else {
        r2 <- annotated_filtered_chains |>
          filter(!!rlang::sym(locus_col) %in% c(locus_2, locus_3))
      }

      receptor_barcode_mapping <- r1 |>
        left_join(
          r2,
          by = immundata_barcode_col
        )

      receptor_chain_mapping <- receptor_barcode_mapping |>
        summarise(
          .by = all_of(paired_receptor_features)
        ) |>
        mutate(
          {{ immundata_receptor_id_col }} := row_number()
        ) |>
        right_join(receptor_barcode_mapping,
          by = paired_receptor_features
        ) |>
        select(all_of(c(immundata_receptor_id_col, paste0(immundata_chain_id_col, c(".x", ".y")))))

      receptor_chain_mapping <- union_all(
        receptor_chain_mapping |> select(all_of(immundata_receptor_id_col), {{ immundata_chain_id_col }} := 2),
        receptor_chain_mapping |> select(all_of(immundata_receptor_id_col), {{ immundata_chain_id_col }} := 3),
      )

      # Step 3: merge back

      annotation_data <- receptor_chain_mapping |>
        left_join(dataset,
          by = immundata_chain_id_col
        ) |>
        mutate(
          {{ immundata_chain_count_col }} := 1,
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
