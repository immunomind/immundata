#' @title Create options for comparing receptor sequences
#'
#' @description
#' Create sequence comparison options for the `seq_options` argument of
#' [filter_immundata()] or [mutate_immundata()]. Use these options to compare a
#' sequence column with one or more reference sequences or patterns.
#'
#' @param query_col Name of the sequence column to compare, such as `"cdr3_aa"`.
#' @param patterns One or more reference sequences or regular-expression
#'   patterns.
#' @param method Comparison method: `"exact"`, `"regex"`, `"lev"`
#'   (Levenshtein distance), or `"hamm"` (Hamming distance). The default is
#'   `"exact"`.
#' @param max_dist Maximum distance accepted by [filter_immundata()] when
#'   `method = "lev"` or `method = "hamm"`. A value is required when filtering
#'   with either distance method. This argument has no effect on
#'   [mutate_immundata()], which reports every calculated distance.
#' @param name_type How result columns created by [mutate_immundata()] are named.
#'   `"index"`, the default, creates short numbered names. `"pattern"` includes
#'   the reference pattern in each name. This argument does not change which
#'   receptors are kept by [filter_immundata()].
#'
#' @return A named list for the `seq_options` argument of [filter_immundata()] or
#'   [mutate_immundata()].
#'
#' @seealso [filter_immundata()], [mutate_immundata()], [annotate_receptors()]
#'
#' @concept utils
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

check_seq_options <- function(seq_options, mode = NULL) {
  checkmate::assert_list(seq_options, null.ok = FALSE)
  checkmate::assert_choice(mode, choices = c("filter", "mutate"), null.ok = FALSE)

  if (!is.null(seq_options$patterns) &&
    length(seq_options$patterns) > 0 &&
    !is.null(seq_options$query_col)) {
    defaults <- list(method = "exact", max_dist = NA, name_type = "index")

    seq_options <- utils::modifyList(defaults, seq_options)

    seq_options$method <- match.arg(seq_options$method, c("exact", "regex", "lev", "hamm"))

    if (mode == "filter" &&
      is.na(seq_options$max_dist) &&
      seq_options$method %in% c("lev", "hamm")) {
      cli::cli_abort("You passed `seq_options` to `filter`, but didn't provide `max_dist` for filtering. Either provide `max_dist` or use `left_join` to annotate receptors with distances to patterns.")
    }

    seq_options
  } else {
    cli::cli_abort("Missing fields in `seq_options`, please use {.run immundata::make_seq_options()} to create the options")
  }
}

make_pattern_columns <- function(patterns,
                                 col_prefix,
                                 name_type = c("pattern", "index")) {
  checkmate::assert_character(patterns, min.len = 1)
  checkmate::assert_character(col_prefix, max.len = 1)
  name_type <- match.arg(name_type)

  sapply(seq_along(patterns), function(p_index) {
    p_seq <- patterns[[p_index]]

    if (name_type == "pattern") {
      safe_name <- gsub("[^A-Za-z0-9]", "_", p_seq) # just in case
      col_name_out <- paste0(col_prefix, safe_name)
    } else if (name_type == "index") {
      col_name_out <- paste0(col_prefix, p_index)
    } else {
      # TODO: what the heck
      stop("!")
    }

    col_name_out
  })
}


#' @keywords internal
annotate_tbl_distance <- function(tbl_data,
                                  query_col,
                                  patterns,
                                  method = c("lev", "hamm"),
                                  max_dist = NA,
                                  name_type = c("pattern", "index")) {
  checkmate::assert_character(query_col, len = 1)
  checkmate::assert_character(patterns, min.len = 1)

  method <- match.arg(method)
  name_type <- match.arg(name_type)

  uniq <- tbl_data |>
    distinct(!!rlang::sym(query_col))

  query_col_expr <- rlang::sym(query_col)

  if (method == "lev") {
    col_prefix <- imd_schema("sim_lev")
  } else if (method == "hamm") {
    col_prefix <- imd_schema("sim_hamm")
  }

  dist_cols <- make_pattern_columns(
    patterns = patterns,
    col_prefix = col_prefix,
    name_type = name_type
  )

  # TODO: Optimize it via SQL instead of cycles - if it is even needed...
  # TODO: lump together multiple patterns in batches
  for (i in seq_along(patterns)) {
    p <- patterns[[i]]
    col_name_out <- dist_cols[i]

    #
    # 1) Levenshtein distance
    #
    if (method == "lev") {
      if (!is.na(max_dist)) {
        len_p <- nchar(p)
        uniq <- uniq |>
          mutate(
            {{ col_name_out }} := dplyr::if_else(
              dd$length(!!query_col_expr) >= len_p - max_dist &
                dd$length(!!query_col_expr) <= len_p + max_dist,
              dd$levenshtein(!!query_col_expr, p),
              NA_real_
            )
          )
      } else {
        uniq <- uniq |>
          mutate(
            {{ col_name_out }} := dd$levenshtein(!!query_col_expr, p)
          )
      }
    }

    #
    # 2) Hamming distance
    #
    else {
      len_p <- nchar(p)
      uniq <- uniq |>
        mutate(
          {{ col_name_out }} := dplyr::if_else(
            dd$length(!!query_col_expr) == len_p,
            dd$hamming(!!query_col_expr, p),
            NA_real_
          )
        )
    }
  }

  #
  # TODO: benchmark 1 - distinct vs no distinct
  # TODO: benchmark 2 - pre-optimize vs no optimize

  # TODO: fun experiment - compute for patterns, then filter out, then compute again, and so on.
  # filter out -> filter out those who has <= max_dist (!) because we already found them and just need to store

  # TODO: Benchmarks
  # 1) distinct vs non-distinct
  # 2) pre-optimize vs no optimization for levenshtein
  # 3) step-by-step filtering out "good" sequences
  # 4) precompute sequence length before (!) any filtering, on data loading, and don't compute it here

  if (!is.na(max_dist)) {
    within_max_dist <- lapply(
      dist_cols,
      function(col) rlang::expr(!!rlang::sym(col) <= !!max_dist)
    ) |>
      Reduce(
        f = function(left, right) rlang::expr((!!left) | (!!right))
      )

    uniq <- uniq |>
      filter(!!within_max_dist)
  }

  uniq |>
    compute(name = basename(tempfile(pattern = "immundata_")))
}


#' @keywords internal
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

  col_prefix <- imd_schema("sim_regex")

  dist_cols <- make_pattern_columns(
    patterns = patterns,
    col_prefix = col_prefix,
    name_type = name_type
  )

  # TODO: Optimize it via SQL instead of cycles - if it is even needed...
  for (i in seq_along(patterns)) {
    p <- patterns[[i]]
    col_name_out <- dist_cols[i]

    # annotate with DuckDB regexp_matches()
    uniq <- uniq |>
      mutate(!!col_name_out := dd$regexp_matches(!!rlang::sym(query_col), p))
  }

  tbl_data <- tbl_data |> left_join(uniq, by = query_col)
  if (filter_out) {
    # TODO: need to replace it with if_else when it is available in duckplyr
    sql_expr <- paste(dist_cols, collapse = " OR ")

    tbl_data |>
      as_tbl() |>
      filter(dbplyr::sql(sql_expr)) |>
      as_duckdb_tibble() |>
      compute() # TODO: We need a compute here because sometimes duckplyr can't find the table
  } else {
    tbl_data
  }
}
