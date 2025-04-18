#' @title Filter ImmunData by Receptor Features (Exact, Fuzzy, or Regex Matching)
#'
#' @description
#' Filter an `ImmunData` object by receptor features, supporting exact, Levenshtein,
#' Hamming, or regular-expression matching on a specified sequence column. User-provided filters
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
#'
#' @return New ImmunData object with both receptors and annotations filtered.
#'
#' @importFrom dplyr filter mutate select semi_join
#' @importFrom checkmate assert_r6 assert_character assert_choice
#' @importFrom rlang sym enquos as_string
#' @importFrom dbplyr sql
filter_receptors <- function(idata, ..., seq_options = NULL) {
  checkmate::assert_r6(idata, "ImmunData")
  dots <- rlang::enquos(...)

  receptor_id <- imd_schema()$receptor

  # Run "basic" filters first
  new_receptors <- idata$receptors
  if (length(dots) > 0) {
    new_receptors <- new_receptors |> filter(!!!dots)
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
      new_receptors <- new_receptors |> filter(!!col_sym %in% seq_options$patterns)

      keep_ids <- new_receptors |> select({{ receptor_id }})

      new_annotations <- idata$annotations |>
        semi_join(keep_ids, by = receptor_id)
    }

    #
    # Regex
    #
    else if (seq_options$method == "regex") {
      new_receptors <- annotate_tbl_regex(new_receptors,
        query_col = seq_options$query_col,
        patterns = seq_options$patterns,
        name_type = seq_options$name_type,
        filter_out = TRUE
      )
    }

    #
    # Levenshtein / hamming
    #
    else {
      new_receptors <- annotate_tbl_distance(
        new_receptors,
        query_col = seq_options$query_col,
        patterns  = seq_options$patterns,
        method    = seq_options$method,
        max_dist  = seq_options$max_dist,
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
      pat_cols <- grep(pat_prefix, colnames(new_receptors), value = TRUE)

      # 2) pull out a tiny table of receptor_id + those columns
      receptor_pattern_tbl <- new_receptors |>
        select({{ receptor_id }}, all_of(pat_cols))

      # 3) drop them from receptors
      new_receptors <- new_receptors |>
        select(-all_of(pat_cols))

      # 4) filter annotations to only the kept receptors
      keep_ids <- new_receptors |> select({{ receptor_id }})

      new_annotations <- idata$annotations |>
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
    keep_ids <- new_receptors |> select({{ receptor_id }})

    new_annotations <- idata$annotations |>
      semi_join(keep_ids, by = receptor_id)
  }

  ImmunData$new(
    receptors = new_receptors,
    annotations = new_annotations,
    schema = idata$schema_receptor
  )
}


#' @title Filter ImmunData by Annotations
#'
#' @description
#' A short description...
#'
#' @param idata [ImmunData] object.
#' @param ... Parameters passed to [dplyr::filter].
#'
#' @export
filter_annotations <- function(idata, ...) {
  checkmate::assert_r6(idata, "ImmunData")

  new_annotations <- idata$annotations |> filter(...)

  receptor_id_col <- imd_schema()$receptor
  receptor_ids <- new_annotations |>
    select({{ receptor_id_col }}) |>
    distinct(!!rlang::sym(receptor_id_col))

  # TODO: do I need to recompute counts / proportions each time?

  new_receptors <- idata$receptors |> semi_join(receptor_ids, by = receptor_id_col)

  ImmunData$new(receptors = new_receptors, annotations = new_annotations, schema = idata$schema_receptor)
}

#' @importFrom checkmate assert
#' @importFrom duckplyr duckdb_tibble
#' @importFrom dplyr distinct
#'
#' @title Filter ImmunData by Passed Cell IDs or Barcodes
#'
#' @description
#' A short description...
#'
#' @param idata [ImmunData] object.
#' @param cells Vector of cell identifiers or barcodes to filter by.
#'
#' @export
filter_cells <- function(idata, cells) {
  # TODO: Figure out what are the options to pass the cells
  # - character vector
  # - something from Seurat / AnnData?

  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert(
    checkmate::check_character(cells, min.len = 1),
    checkmate::check_integer(cells, min.len = 1),
    checkmate::check_double(cells, min.len = 1)
  )

  barcode_col_id <- imd_schema()$cell
  barcodes_table <- duckdb_tibble(A = unique(cells))
  colnames(barcodes_table) <- barcode_col_id

  new_annotations <- idata$annotations |> semi_join(barcodes_table, by = barcode_col_id)

  # TODO: do I need to recompute proportions each time?

  receptor_id_col <- imd_schema()$receptor
  receptor_ids <- new_annotations |>
    select({{ receptor_id_col }}) |>
    distinct(!!rlang::sym(receptor_id_col))

  new_receptors <- idata$receptors |> semi_join(receptor_ids, by = receptor_id_col)

  ImmunData$new(receptors = new_receptors, annotations = new_annotations, schema = idata$schema_receptor)
}
