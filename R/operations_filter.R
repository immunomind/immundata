#' @title Filter ImmunData by receptor features, barcodes or any annotations
#'
#' @description
#' Provides flexible filtering options for an `ImmunData` object.
#'
#' `filter()` is the main function, allowing filtering based on receptor features
#' (e.g., CDR3 sequence) using various matching methods (exact, regex, fuzzy) and/or
#' standard `dplyr`-style filtering on annotation columns.
#'
#' `filter_barcodes()` is a convenience function to filter by specific cell barcodes.
#'
#' `filter_receptors()` is a convenience function to filter by specific receptor identifiers.
#'
#' @details
#' For `filter`:
#' * User-provided `dplyr`-style filters (`...`) are applied *before* any sequence-based
#'     filtering defined in `seq_options`.
#' * Sequence filtering compares values in the `query_col` of the annotations table
#'     against the provided `patterns`.
#' * Supported sequence matching methods are:
#'     * `"exact"`: Keeps rows where `query_col` exactly matches any of the `patterns`.
#'     * `"regex"`: Keeps rows where `query_col` matches any of the regular expressions
#'         in `patterns`.
#'     * `"lev"` (Levenshtein distance): Keeps rows where the edit distance between
#'         `query_col` and any pattern is less than or equal to `max_dist`.
#'     * `"hamm"` (Hamming distance): Keeps rows where the Hamming distance (for
#'         equal length strings) between `query_col` and any pattern is less than
#'         or equal to `max_dist`.
#' * The filtering operations act on the `$annotations` table. A new `ImmunData`
#'     object is created containing only the rows (and corresponding receptors)
#'     that pass the filter(s).
#' * If `keep_repertoires = TRUE` (and repertoire data exists in the input),
#'     the repertoire-level summaries (`$repertoires` table) are recalculated based
#'     on the filtered annotations. Otherwise, the `$repertoires` table in the
#'     output will be `NULL`.
#'
#' For `filter_barcodes` and `filter_receptors`:
#' * These functions provide a simpler interface for common filtering tasks based on
#'     cell barcodes or receptor IDs, respectively. They use efficient `semi_join`
#'     operations internally.
#'
#' @param idata,.data An `ImmunData` object.
#' @param ... For `filter`, these are regular `dplyr`-style filtering
#'   expressions (e.g., `V_gene == "IGHV1-1"`, `chain == "IGH"`) applied to the
#'   `$annotations` table *before* sequence filtering. Ignored by `filter_barcodes`
#'   and `filter_receptors`.
#' @param .by Not used.
#' @param .preserve Not used.
#' @param seq_options For `filter`, an optional named list specifying sequence-based
#'   filtering options. Use [make_seq_options()] for convenient creation.
#'   The list can contain:
#'   * `query_col` (Character scalar): The name of the column in `$annotations`
#'       containing sequences to compare (e.g., `"CDR3_aa"`, `"FR1_nt"`).
#'   * `patterns` (Character vector): A vector of sequences or regular expressions
#'       to match against `query_col`.
#'   * `method` (Character scalar): The matching method. One of `"exact"`,
#'       `"regex"`, `"lev"` (Levenshtein distance), or `"hamm"` (Hamming distance).
#'       Defaults typically handled by `make_seq_options`.
#'   * `max_dist` (Numeric scalar): For fuzzy methods (`"lev"`, `"hamm"`), the
#'       maximum allowed distance. Rows with distance <= `max_dist` are kept.
#'       Defaults typically handled by `make_seq_options`.
#'   * `name_type` (Character scalar): Determines column names in intermediate distance
#'        calculations if applicable (`"index"` or `"pattern"`). Passed through to
#'        internal annotation functions. Defaults typically handled by `make_seq_options`.
#'   If `seq_options` is `NULL` (the default), no sequence-based filtering is performed.
#' @param keep_repertoires Logical scalar. If `TRUE` (the default) and the input
#'   `idata` has repertoire information (`idata$schema_repertoire` is not `NULL`),
#'   the repertoire summaries will be recalculated based on the filtered data using
#'   [agg_repertoires()]. If `FALSE`, or if no repertoire schema exists, the
#'   returned `ImmunData` object will not contain repertoire summaries (`$repertoires`
#'   will be `NULL`).
#' @param barcodes For `filter_barcodes`, a vector of cell identifiers (barcodes)
#'   to keep. Can be character, integer, or numeric.
#' @param receptors For `filter_receptors`, a vector of receptor identifiers
#'   to keep. Can be character, integer, or numeric.
#'
#' @return A new `ImmunData` object containing only the filtered annotations
#'   (and potentially recalculated repertoire summaries). The schema remains the same.
#'
#' @seealso [make_seq_options()], [dplyr::filter()], [agg_repertoires()], [ImmunData]
#'
#' @examples
#' # Basic setup (assuming idata_test is a valid ImmunData object)
#' # print(idata_test)
#'
#' # --- filter examples ---
#' \dontrun{
#' # Example 1: dplyr-style filtering on annotations
#' filtered_heavy <- filter(idata_test, chain == "IGH")
#' print(filtered_heavy)
#'
#' # Example 2: Exact sequence matching on CDR3 amino acid sequence
#' cdr3_patterns <- c("CARGLGLVFYGMDVW", "CARDNRGAVAGVFGEAFYW")
#' seq_opts_exact <- make_seq_options(query_col = "CDR3_aa", patterns = cdr3_patterns)
#' filtered_exact_cdr3 <- filter(idata_test, seq_options = seq_opts_exact)
#' print(filtered_exact_cdr3)
#'
#' # Example 3: Combining dplyr-style and fuzzy sequence matching (Levenshtein)
#' seq_opts_lev <- make_seq_options(
#'   query_col = "CDR3_aa",
#'   patterns = "CARGLGLVFYGMDVW",
#'   method = "lev",
#'   max_dist = 1
#' )
#' filtered_combined <- filter(idata_test,
#'   chain == "IGH",
#'   C_gene == "IGHG1",
#'   seq_options = seq_opts_lev
#' )
#' print(filtered_combined)
#'
#' # Example 4: Regex matching on V gene
#' v_gene_pattern <- "^IGHV[13]-" # Keep only IGHV1 or IGHV3 families
#' seq_opts_regex <- make_seq_options(
#'   query_col = "V_gene",
#'   patterns = v_gene_pattern,
#'   method = "regex"
#' )
#' filtered_regex_v <- filter(idata_test, seq_options = seq_opts_regex)
#' print(filtered_regex_v)
#'
#' # Example 5: Filtering without recalculating repertoires
#' filtered_no_rep <- filter(idata_test, chain == "IGK", keep_repertoires = FALSE)
#' print(filtered_no_rep) # $repertoires should be NULL
#' }
#'
#' # --- filter_barcodes example ---
#' \dontrun{
#' # Assuming 'cell1_barcode' and 'cell5_barcode' exist in idata_test$annotations$cell_id
#' specific_barcodes <- c("cell1_barcode", "cell5_barcode")
#' filtered_cells <- filter_barcodes(idata_test, barcodes = specific_barcodes)
#' print(filtered_cells)
#' }
#'
#' # --- filter_receptors example ---
#' \dontrun{
#' # Assuming receptor IDs 101 and 205 exist in idata_test$annotations$receptor_id
#' specific_receptors <- c(101, 205) # Or character IDs if applicable
#' filtered_recs <- filter_receptors(idata_test, receptors = specific_receptors)
#' print(filtered_recs)
#' }
#'
#' @concept filtering
#' @export
filter_immundata <- function(idata, ..., seq_options = NULL, keep_repertoires = TRUE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_list(seq_options, null.ok = TRUE)
  checkmate::assert_logical(keep_repertoires)

  dots <- rlang::enquos(...)

  receptor_id <- imd_schema()$receptor

  # Run "basic" filters first
  new_annotations <- idata$annotations
  if (length(dots) > 0) {
    new_annotations <- new_annotations |> filter(!!!dots)
  }

  #
  # With sequence filters
  #
  if (!is.null(seq_options)) {
    seq_options <- check_seq_options(seq_options, mode = "filter")

    col_sym <- rlang::sym(seq_options$query_col)

    #
    # Exact
    #
    if (seq_options$method == "exact") {
      new_annotations <- new_annotations |> filter(!!col_sym %in% seq_options$patterns)

      keep_ids <- new_annotations |> select({{ receptor_id }})

      new_annotations <- idata$annotations |>
        semi_join(keep_ids, by = receptor_id)
    }

    #
    # Regex
    #
    else if (seq_options$method == "regex") {
      distance_data <- annotate_tbl_regex(
        new_annotations |> select(!!col_sym),
        query_col = seq_options$query_col,
        patterns = seq_options$patterns,
        filter_out = TRUE,
        name_type = seq_options$name_type
      )
    }

    #
    # Levenshtein / hamming
    #
    else {
      distance_data <- annotate_tbl_distance(
        new_annotations |> select(!!col_sym),
        query_col = seq_options$query_col,
        patterns = seq_options$patterns,
        method = seq_options$method,
        max_dist = seq_options$max_dist,
        name_type = seq_options$name_type
      )
    }

    #
    # Select only those receptors which passed the filer
    #
    # TODO: Refactor, but I'm not sure how to do it properly. Simply split to separte functions <compute distance> + <filter immundata>?
    # TODO: looks like a case for <move_annotations> from receptors to annotations
    if (seq_options$method != "exact") {
      new_annotations <- new_annotations |>
        semi_join(distance_data, by = seq_options$query_col)
    }
  }

  #
  # No sequence filters
  #
  else {
    keep_ids <- new_annotations |> select({{ receptor_id }})

    new_annotations <- new_annotations |>
      semi_join(keep_ids, by = receptor_id)
  }

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations
  )

  if (keep_repertoires && !is.null(idata$schema_repertoire)) {
    new_idata |> agg_repertoires(idata$schema_repertoire)
  } else {
    new_idata
  }
}

#' @rdname filter_immundata
#' @exportS3Method dplyr::filter
filter.ImmunData <- function(.data, ..., .by = NULL, .preserve = FALSE, seq_options = NULL, keep_repertoires = TRUE) {
  filter_immundata(idata = .data, ..., seq_options = seq_options, keep_repertoires = keep_repertoires)
}

#' @rdname filter_immundata
#' @export
filter_barcodes <- function(idata, barcodes, keep_repertoires = TRUE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert(
    checkmate::check_character(barcodes, min.len = 1),
    checkmate::check_integer(barcodes, min.len = 1),
    checkmate::check_double(barcodes, min.len = 1)
  )
  checkmate::assert_logical(keep_repertoires)

  barcode_col_id <- imd_schema()$cell
  barcodes_table <- duckdb_tibble(A = unique(barcodes))
  colnames(barcodes_table) <- barcode_col_id

  new_annotations <- idata$annotations |> semi_join(barcodes_table, by = barcode_col_id)

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations
  )

  if (keep_repertoires && !is.null(idata$schema_repertoire)) {
    new_idata |> agg_repertoires(idata$schema_repertoire)
  } else {
    new_idata
  }
}


#' @rdname filter_immundata
#' @export
filter_receptors <- function(idata, receptors, keep_repertoires = TRUE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert(
    checkmate::check_character(receptors, min.len = 1),
    checkmate::check_integer(receptors, min.len = 1),
    checkmate::check_double(receptors, min.len = 1)
  )
  checkmate::assert_logical(keep_repertoires)

  receptors_col_id <- imd_schema()$receptor
  receptors_table <- duckdb_tibble(A = unique(receptors))
  colnames(receptors_table) <- receptors_col_id

  new_annotations <- idata$annotations |> semi_join(receptors_table, by = receptors_col_id)

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations
  )

  if (keep_repertoires && !is.null(idata$schema_repertoire)) {
    new_idata |> agg_repertoires(idata$schema_repertoire)
  } else {
    new_idata
  }
}
