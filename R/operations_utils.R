#' @title Build a `seq_options` list for sequence‑based receptor filtering
#'
#' @description
#' A convenience wrapper that validates the common arguments for
#' **`filter_receptors()`** and returns them in the required list form.
#'
#' @param query_col Character(1). Name of the receptor column to compare
#'   (e.g. `"cdr3_aa"`).
#' @param patterns  Character vector of sequences or regular expressions to
#'   search for.
#' @param method    One of `"exact"`, `"regex"`, `"lev"` (Levenshtein), or
#'   `"hamm"` (Hamming).  Defaults to `"exact"`.
#' @param max_dist  Numeric distance threshold for `"lev"` / `"hamm"`
#'   filtering.  Use `NA` (default) to keep all rows after annotation.
#' @param name_type Passed straight to `annotate_tbl_distance()`; either
#'   `"index"` (default) or `"pattern"`.
#'
#' @return A named list suitable for the `seq_options` argument of
#'   [filter_receptors()].
#'
#' @seealso [filter_receptors()], [annotate_receptors()]
#' @export
make_seq_options <- function(query_col,
                             patterns,
                             method = c("exact", "lev", "hamm", "regex"),
                             max_dist = NA,
                             name_type = c("index", "pattern")) {
  checkmate::assert_character(query_col, len = 1)
  checkmate::assert_character(patterns, min.len = 1)

  list(
    query_col = query_col,
    patterns  = patterns,
    method    = match.arg(method),
    max_dist  = max_dist,
    name_type = match.arg(name_type)
  )
}


annotate_tbl_distance <- function(tbl_data,
                                  query_col,
                                  patterns,
                                  method = c("lev", "hamm"),
                                  max_dist = NA,
                                  name_type = c("pattern", "index")) {
  assert_character(query_col, len = 1)
  assert_character(patterns, min.len = 1)

  method <- match.arg(method)
  name_type <- match.arg(name_type)

  uniq <- tbl_data |>
    distinct(!!rlang::sym(query_col)) |>
    as_tbl() |>
    mutate(
      kmer_left = dbplyr::sql(cli::format_inline("{query_col}[:3]")),
      kmer_right = dbplyr::sql(cli::format_inline("{query_col}[-2:]"))
    )

  # TODO: use globals for col prefix
  col_prefix <- paste0("imd_dist_", method, "_")
  dist_cols <- c()

  # TODO: Optimize it via SQL instead of cycles - if it is even needed...
  for (i in seq_along(patterns)) {
    p <- patterns[[i]]

    if (name_type == "pattern") {
      safe_name <- gsub("[^A-Za-z0-9]", "_", p) # just in case
      col_name_out <- paste0(col_prefix, safe_name)
    } else {
      col_name_out <- paste0(col_prefix, i)
    }

    dist_cols <- c(dist_cols, col_name_out)

    #
    # 1) Levenshtein distance
    #
    if (method == "lev") {
      len_p <- nchar(p)
      sql_expr <- cli::format_inline(
        "CASE WHEN ",
        " kmer_left = {query_col}[:3] AND kmer_right = {query_col}[-2:] AND",
        " length({query_col}) >= {len_p - max_dist} AND length({query_col}) <= {len_p + max_dist}",
        " THEN levenshtein({query_col}, '{p}')",
        " ELSE NULL END"
      )

      uniq <- uniq |>
        mutate({{ col_name_out }} := dbplyr::sql(sql_expr))
    }

    #
    # 2) Hamming distance
    #
    else {
      len_p <- nchar(p)
      sql_expr <- cli::format_inline(
        "CASE WHEN length({query_col}) = {len_p}",
        " THEN hamming({query_col}, '{p}')",
        " ELSE NULL END"
      )

      uniq <- uniq |>
        mutate({{ col_name_out }} := dbplyr::sql(sql_expr))
    }
  }

  #
  # TODO: In case of max_dist, pre-optimize for levenshtein by filtering out too short or too long distances
  # TODO: benchmark 1 - distinct vs no distinct
  # TODO: benchmark 2 - pre-optimize vs no optimize

  # TODO: fun experiment - compute for patterns, then filter out, then compute again, and so on.
  # filter out -> filter out those who has <= max_dist (!) because we already found them and just need to store

  # TODO: Benchmarks
  # 1) distinct vs non-distinct
  # 2) pre-optimize vs no optimization for levenshtein
  # 3) step-by-step filtering out "good" sequences
  # 4) precompute sequence length before (!) any filtering, on data loading, and don't compute it here

  # TODO: max dist. Left join - compute. Right join - filter
  if (is.na(max_dist)) {
    uniq <- uniq |>
      as_duckdb_tibble() |>
      compute()
    uniq
  } else {
    sql_expr <- sprintf("LEAST(%s) <= %d", paste(dist_cols, collapse = ", "), max_dist)

    uniq <- uniq |>
      filter(dbplyr::sql(sql_expr)) |>
      as_duckdb_tibble()

    output <- uniq |>
      left_join(tbl_data, by = query_col) |>
      compute()

    output
  }
}


#' @title Annotate a Duckplyr Table with Regex Match Flags
#'
#' @description
#' Adds one logical “does this sequence match this regex?” column per pattern,
#' using DuckDB’s `regexp_matches()` via the `dd` escape hatch.
#'
#' @param tbl_data   A duckplyr table (e.g. `idata$receptors`).
#' @param query_col  Character(1). Name of the sequence column to search.
#' @param patterns   Character vector of regular expressions.
#' @param filter_out Logical(1). If TRUE, filter out sequences which don't match the pattern.
#' @param name_type  One of `"pattern"` or `"index"`:
#'                   - `"pattern"`: column names `match_regex_<sanitized_pattern>`
#'                   - `"index"`:   column names `match_regex_<i>
#'
#' @return The input table with new logical match columns added.
#' @importFrom dplyr distinct mutate left_join
#' @importFrom checkmate assert_character
#' @importFrom rlang sym
annotate_tbl_regex <- function(tbl_data,
                               query_col,
                               patterns,
                               filter_out = FALSE,
                               name_type = c("index", "pattern")) {
  checkmate::assert_character(query_col, len = 1)
  checkmate::assert_character(patterns, min.len = 1)
  checkmate::assert_logical(filter_out)

  name_type <- match.arg(name_type)

  uniq <- tbl_data |> distinct(!!rlang::sym(query_col))
  col_prefix <- "match_regex_"
  pattern_cols <- c()

  for (i in seq_along(patterns)) {
    pat <- patterns[[i]]

    # choose output column name
    if (name_type == "pattern") {
      safe_name <- gsub("[^A-Za-z0-9]", "_", p) # just in case
      col_name_out <- paste0(col_prefix, safe_name)
    } else {
      col_name_out <- paste0(col_prefix, i)
    }

    pattern_cols <- c(pattern_cols, col_name_out)

    # annotate with DuckDB regexp_matches()
    uniq <- uniq |>
      mutate(!!col_name_out := dd$regexp_matches(!!rlang::sym(query_col), pat))
  }

  tbl_data <- tbl_data |> left_join(uniq, by = query_col)
  if (filter_out) {
    # TODO: need to replace it with if_else when it is available in duckplyr
    sql_expr <- paste(pattern_cols, collapse = " OR ")

    tbl_data |>
      as_tbl() |>
      filter(dbplyr::sql(sql_expr)) |>
      as_duckdb_tibble() |>
      compute() # TODO: We need a compute here because sometimes duckplyr can't find the table
  } else {
    tbl_data
  }
}

#' @importFrom rlang sym
to_sym <- function(val) {
  if (length(val) == 1) {
    rlang::sym(val)
  } else {
    rlang::syms(val)
  }
}
