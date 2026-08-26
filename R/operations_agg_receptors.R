#' @title Group AIRR sequence rows into receptors
#'
#' @description
#' `agg_receptors()` is a low-level function used during AIRR data ingestion. It
#' decides which sequence rows represent the same biological receptor and adds
#' package-standard identifiers and counts to the input table.
#'
#' A receptor can be one chain or a pair of chains from the same cell. The
#' `schema` argument defines which sequence features and loci make two receptors
#' identical.
#'
#' This function works with a prepared duckplyr table and returns a duckplyr
#' table. It does not accept or return an [ImmunData] object. Most analysis
#' workflows should provide the same arguments to [read_repertoires()], which
#' calls `agg_receptors()` during import.
#'
#' @param dataset A duckplyr table containing AIRR sequence data, with one row
#'   per chain or bulk clonotype. It must contain the columns named in `schema`
#'   and in any of `barcode_col`, `count_col`, `locus_col`, and `umi_col` that
#'   are supplied.
#' @param schema Definition of receptor identity. Supply either:
#'
#'   * A character vector naming the features that must match, such as
#'     `c("v_call", "j_call", "junction_aa")`.
#'   * An object created by [make_receptor_schema()]. Its `features` define chain
#'     identity, while its optional `chains` select one locus or define a pair
#'     of loci.
#'
#'   A schema can contain at most two chain entries. Use syntax such as
#'   `c("IGH", "IGL|IGK")` to accept either IGH-IGL or IGH-IGK pairs.
#' @param barcode_col Name of the column containing cell barcodes. Supply this
#'   for single-cell data. `umi_col` is then also required, and `count_col`
#'   cannot be supplied. If `imd_filename` is present, identical barcode values
#'   from different source files are treated as different cells. The default is
#'   `NULL`.
#' @param count_col Name of the column containing non-negative abundance values
#'   in bulk repertoire data. These values are copied to `imd_n_chains`.
#'   `count_col` cannot be used together with `barcode_col`. The default is
#'   `NULL`.
#' @param locus_col Name of the column containing loci such as `"TRA"`, `"TRB"`,
#'   or `"IGH"`. It is required when `schema` specifies one or more chains. The
#'   column is renamed to the standard name `locus` when necessary. The default
#'   is `NULL`.
#' @param umi_col Name of the column containing per-chain UMI or read counts.
#'   It is required when `barcode_col` is supplied and is used to choose one
#'   chain when a cell contains several chains from the same locus. The default
#'   is `NULL`.
#' @param verbose Whether to print information about the selected processing
#'   mode and loci. Defaults to `getOption("immundata.verbose", TRUE)`.
#'
#' @details
#' The receptor features are the columns that define the identity of one chain.
#' Two chains with the same values in all feature columns receive the same
#' receptor identity in a single-chain analysis. Common features include V gene,
#' J gene, and CDR3 amino acid sequence. Rows with a missing, empty, or
#' whitespace-only receptor feature are excluded before chains are selected.
#'
#' The function supports three input modes:
#'
#' * **Uncounted sequence table:** If neither `barcode_col` nor `count_col` is
#'   supplied, every input row is treated as one observed chain. A synthetic
#'   barcode is created for each row, and `imd_n_chains` is set to `1`.
#' * **Bulk repertoire:** If `count_col` is supplied, every input row receives a
#'   synthetic barcode and its abundance is copied to `imd_n_chains`.
#' * **Single-cell repertoire:** If `barcode_col` is supplied, rows are grouped
#'   by cell. `umi_col` is required and `imd_n_chains` is set to `1` for every
#'   retained cell-chain observation.
#'
#' When one chain is specified in `schema`, only that locus is retained. If a
#' cell contains several chains from that locus, the row with the highest value
#' in `umi_col` is retained. If the highest values are tied, the first row is
#' retained.
#'
#' When two chains are specified, only cells containing both requested loci are
#' retained. The selected chains are paired by barcode, and both rows receive
#' the same `imd_receptor_id`. Cells with incomplete pairs are excluded.
#'
#' A relaxed pair such as `c("IGH", "IGL|IGK")` requires IGH and exactly one of
#' the two alternative light-chain loci. Cells containing both IGL and IGK are
#' excluded.
#'
#' Numeric `imd_receptor_id` values identify receptors within the returned
#' table. The particular number assigned to a receptor is not a biological
#' identifier and may change when the data are aggregated again.
#'
#' @return A duckplyr table containing the retained input rows and these
#'   package-standard columns:
#'
#'   * `imd_receptor_id`: links rows that belong to the same receptor.
#'   * `imd_barcode`: contains the input cell barcode, or a synthetic row-level
#'     barcode for uncounted and bulk data.
#'   * `imd_chain_id`: identifies an individual retained chain row.
#'   * `imd_n_chains`: contains `1` for uncounted and single-cell data, or the
#'     value from `count_col` for bulk data.
#'   * `imd_count`: initialized to `0`; receptor counts are calculated later by
#'     [agg_repertoires()].
#'
#' @seealso [read_repertoires()], [make_receptor_schema()], [agg_repertoires()],
#'   [ImmunData]
#'
#' @concept aggregation
#' @export
agg_receptors <- function(dataset, schema, barcode_col = NULL, count_col = NULL, locus_col = NULL, umi_col = NULL,
                          verbose = getOption("immundata.verbose", TRUE)) {
  checkmate::assert_data_frame(dataset)
  checkmate::assert_string(barcode_col, min.chars = 1, null.ok = TRUE)
  checkmate::assert_string(count_col, min.chars = 1, null.ok = TRUE)
  checkmate::assert_string(locus_col, min.chars = 1, null.ok = TRUE)
  checkmate::assert_string(umi_col, min.chars = 1, null.ok = TRUE)
  checkmate::assert_flag(verbose)

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

  if (!is.null(count_col)) {
    negative_count_summary <- dataset |>
      filter(!!rlang::sym(count_col) < 0) |>
      summarise(n_negative = n()) |>
      collect()

    if (negative_count_summary$n_negative[[1]] > 0) {
      cli::cli_abort(
        "Bulk counts in {.field {count_col}} must be non-negative."
      )
    }
  }

  # TODO:
  #   if (checkmate::test_r6(idata, "ImmunData")) {
  #     dataset <- idata$annotations
  #   } else {
  #     dataset <- idata
  #   }

  immundata_barcode_col <- imd_schema("barcode")
  immundata_filename_col <- imd_schema("manifest_filename")
  immundata_receptor_id_col <- imd_schema("receptor")
  immundata_chain_id_col <- imd_schema("chain")
  immundata_count_col <- imd_schema("count")
  immundata_chain_count_col <- imd_schema("chain_count")

  # TODO: refactor
  if (!is.null(locus_col)) {
    canonical_locus_col <- imd_schema("locus")

    if (locus_col != canonical_locus_col) {
      original_locus_col <- locus_col

      if (canonical_locus_col %in% colnames(dataset)) {
        cli::cli_abort(
          "Cannot standardize {.arg locus_col}: the dataset contains both the custom locus column {.field {original_locus_col}} and the canonical locus column {.field {canonical_locus_col}}."
        )
      }

      if (verbose) {
        cli::cli_alert_info("Renaming {original_locus_col} to {canonical_locus_col}")
      }

      dataset <- dataset |>
        rename(!!canonical_locus_col := all_of(original_locus_col))
      locus_col <- canonical_locus_col
    }
  }

  # Prefilter locus
  if (is.null(receptor_chains)) {
    if (verbose) {
      cli::cli_alert_info("No locus information found")
    }
  } else if (length(parsed_chains) == 1) {
    dataset <- dataset |> filter(!!rlang::sym(locus_col) == parsed_chains)
    if (verbose) {
      cli::cli_alert_info("Found target locus: {parsed_chains}. The dataset will be pre-filtered to leave chains for this locus only")
    }
  } else {
    dataset <- dataset |> filter(!!rlang::sym(locus_col) %in% parsed_chains)
    if (verbose) {
      if (is_relaxed_pairing) {
        cli::cli_alert_info("Found relaxed locus pair: {receptor_chains[1]} + ({receptor_chains[2]}). The dataset will be pre-filtered to leave chains for these loci only")
      } else {
        cli::cli_alert_info("Found locus pair: {receptor_chains}. The dataset will be pre-filtered to leave chains for these loci only")
      }
    }
  }

  complete_receptor_features <- lapply(
    rlang::syms(receptor_features),
    function(feature) {
      rlang::expr(!is.na(!!feature) & dd$trim(!!feature) != "")
    }
  )
  dataset <- dataset |>
    filter(!!!complete_receptor_features)


  #
  # 1) Case #1: simple receptor table - no barcodes, no count column
  #
  if (is.null(barcode_col) && is.null(count_col)) {
    if (verbose) {
      cli::cli_alert_info("Processing data as immune repertoire tables - no counts, no barcodes, no chain pairing possible")
    }

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
    if (verbose) {
      cli::cli_alert_info("Processing data as bulk sequencing immune repertoires - with counts, no barcodes, no chain pairing possible")
    }

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
    if (verbose) {
      cli::cli_alert_info("Processing data as single-cell sequencing immune repertoires - no counts, with barcodes, chain pairing is possible")
    }

    dataset <- dataset |>
      mutate(
        {{ immundata_barcode_col }} := !!rlang::sym(barcode_col),
        {{ immundata_chain_id_col }} := row_number()
      )

    # Raw barcodes are only unique within their source library. Scope all
    # single-cell selection and pairing operations by the source filename when
    # it is available, while retaining the raw barcode in `imd_barcode`.
    cell_group_cols <- c(
      if (immundata_filename_col %in% colnames(dataset)) immundata_filename_col,
      immundata_barcode_col
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
          cell_group_cols,
          umi_col
        ))) |>
        mutate(
          .by = all_of(cell_group_cols),
          temp__reads = max(!!rlang::sym(umi_col), na.rm = TRUE)
        ) |>
        filter(!!rlang::sym(umi_col) == temp__reads) |>
        distinct(!!!rlang::syms(cell_group_cols), .keep_all = TRUE) |>
        select(all_of(c(cell_group_cols, immundata_chain_id_col)))

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
          cell_group_cols,
          umi_col,
          locus_col
        ))) |>
        mutate(
          .by = all_of(c(cell_group_cols, locus_col)),
          temp__reads = max(!!rlang::sym(umi_col), na.rm = TRUE)
        ) |>
        filter(!!rlang::sym(umi_col) == temp__reads) |>
        # If there are ties, keep the first one
        distinct(!!!rlang::syms(c(cell_group_cols, locus_col)), .keep_all = TRUE) |>
        select(all_of(c(cell_group_cols, locus_col, immundata_chain_id_col)))

      if (!is_relaxed_pairing) {
        # - find barcodes with both loci
        valid_barcodes <- filtered_chains |>
          summarise(
            .by = all_of(cell_group_cols),
            n = n()
          ) |>
          filter(n == 2)
      } else {
        # - find barcodes with one main locus and only one of the alternative loci
        valid_barcodes <- filtered_chains |>
          summarise(
            .by = all_of(cell_group_cols),
            has_l1 = any(!!rlang::sym(locus_col) == locus_1),
            has_l2 = any(!!rlang::sym(locus_col) == locus_2),
            has_l3 = any(!!rlang::sym(locus_col) == locus_3),
          ) |>
          filter(.data$has_l1, (.data$has_l2 & !.data$has_l3) | (!.data$has_l2 & .data$has_l3))
      }

      # - get back to chains to select only those which are paired
      filtered_chains <- filtered_chains |>
        semi_join(valid_barcodes,
          by = cell_group_cols
        )

      # Looks like back-and-forth, but I'm not sure how to make it better, tbh
      # Alternative: filter out bad barcodes first, but it would require n_distinct
      # as a first step, so pretty much the same as currently.
      # TODO: benchmark this

      # Step 2: create receptors and their identifiers by self-join

      annotated_filtered_chains <- dataset |>
        select(all_of(c(receptor_features, locus_col, immundata_chain_id_col, cell_group_cols))) |>
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
          by = cell_group_cols
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
