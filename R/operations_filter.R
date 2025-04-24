#' @title Filter ImmunData by Receptor Features or Annotations
#'
#' @description
#' Filter an `ImmunData` object by receptor features or annotations, supporting exact, Levenshtein,
#' Hamming, or regular-expression matching on a specified column with sequences, e.g., CDR3 or FR1. User-provided filters
#' (`...`) are applied before any patterns-based matching.
#'
#' @param idata      ImmunData object
#' @param ...        Regular dplyr‑style filtering expressions (applied first)
#' @param seq_options Optional named list with (any subset of)
#'   * `query_col` – column to compare (character, length 1)
#'   * `patterns`  – character vector of sequences / regexes
#'   * `method`    – `"exact"` | `"regex"` | `"lev"` | `"hamm"`
#'   * `max_dist`  – numeric; for fuzzy methods, keep rows ≤ `max_dist`
#'   * `name_type` – `"index"` | `"pattern"` (passed through)
#'   If `seq_options` is `NULL` no sequence filtering is done. Use [make_seq_options()] for convenience.
#' @param receptors Vector of receptor identifiers to filter by.
#' @param cells Vector of cell identifiers or barcodes to filter by.
#'
#' @return New ImmunData object with both receptors and annotations filtered.
#'
#' @concept Filtering
#' @export
filter.ImmunData <- function(idata, ..., seq_options = NULL, keep_repertoires = TRUE) {
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
  if (!is.null(seq_options) &&
    !is.null(seq_options$patterns) &&
    length(seq_options$patterns) > 0 &&
    !is.null(seq_options$query_col)) {
    defaults <- list(method = "exact", max_dist = NA, name_type = "index")
    seq_options <- utils::modifyList(defaults, seq_options)
    seq_options$method <- match.arg(seq_options$method, c("exact", "regex", "lev", "hamm"))

    if (is.na(seq_options$max_dist) && seq_options$method %in% c("lev", "hamm")) {
      cli::cli_abort("You passed `seq_options` to `filter_receptors`, but didn't provide `max_dist` for filtering. Either provide `max_dist` or use `annotate_receptors` to annotate receptors with distances to patterns.")
    }

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
      filtered_data <- annotate_tbl_regex(
        new_annotations |>
          select(!!rlang::sym(receptor_id), !!col_sym),
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
      filtered_data <- annotate_tbl_distance(
        new_annotations |>
          select(!!rlang::sym(receptor_id), !!col_sym),
        query_col = seq_options$query_col,
        patterns = seq_options$patterns,
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
      # 1) figure out the prefix for the columns we want to move
      pat_prefix <- switch(seq_options$method,
        regex = "match_regex_",
        lev   = "imd_dist_lev_",
        hamm  = "imd_dist_hamm_"
      )
      pat_cols <- grep(pat_prefix, colnames(filtered_data), value = TRUE)

      # 2) pull out a tiny table of receptor_id + those columns
      receptor_pattern_tbl <- filtered_data |>
        select({{ receptor_id }}, all_of(pat_cols))

      # 3) drop them from receptors
      filtered_data <- filtered_data |>
        select(-all_of(pat_cols))

      # 4) filter annotations to only the kept receptors
      keep_ids <- filtered_data |> select({{ receptor_id }})

      new_annotations <- new_annotations |>
        semi_join(keep_ids, by = receptor_id)

      # 5) stitch the pattern columns onto annotations
      new_annotations <- new_annotations |>
        left_join(receptor_pattern_tbl, by = receptor_id)
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

#' @concept Filtering
#' @rdname filter.ImmunData
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

#' @concept Filtering
#' @rdname filter.ImmunData
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
