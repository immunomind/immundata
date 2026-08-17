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
    distinct(!!rlang::sym(query_col)) |>
    as_tbl()

  con <- dbplyr::remote_con(uniq)
  query_col_sql <- dbplyr::escape(
    dbplyr::ident(query_col),
    con = con
  )

  # TODO: settings for kmers
  if (!is.na(max_dist)) {
    kmer_left_sql <- dbplyr::build_sql(
      query_col_sql,
      "[:3]",
      con = con
    )
    kmer_right_sql <- dbplyr::build_sql(
      query_col_sql,
      "[-2:]",
      con = con
    )

    uniq <- uniq |>
      mutate(
        kmer_left = dbplyr::sql(kmer_left_sql),
        kmer_right = dbplyr::sql(kmer_right_sql)
      )
  }

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
    pattern_sql <- dbplyr::escape(p, con = con)
    col_name_out <- dist_cols[i]

    #
    # 1) Levenshtein distance
    #
    if (method == "lev") {
      if (!is.na(max_dist)) {
        len_p <- nchar(p)
        sql_expr <- dbplyr::build_sql(
          "CASE WHEN kmer_left = ", query_col_sql, "[:3]",
          " AND kmer_right = ", query_col_sql, "[-2:]",
          " AND length(", query_col_sql, ") >= ", len_p - max_dist,
          " AND length(", query_col_sql, ") <= ", len_p + max_dist,
          " THEN levenshtein(", query_col_sql, ", ", pattern_sql, ")",
          " ELSE NULL END",
          con = con
        )
      } else {
        sql_expr <- dbplyr::build_sql(
          "levenshtein(", query_col_sql, ", ", pattern_sql, ")",
          con = con
        )
      }

      uniq <- uniq |>
        mutate({{ col_name_out }} := dbplyr::sql(sql_expr))
    }

    #
    # 2) Hamming distance
    #
    else {
      len_p <- nchar(p)
      sql_expr <- dbplyr::build_sql(
        "CASE WHEN length(", query_col_sql, ") = ", len_p,
        " THEN hamming(", query_col_sql, ", ", pattern_sql, ")",
        " ELSE NULL END",
        con = con
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
      as_duckdb_tibble()
  } else {
    dist_cols_sql <- dbplyr::escape(
      dbplyr::ident(dist_cols),
      collapse = ", ",
      con = con
    )
    sql_expr <- dbplyr::build_sql(
      "LEAST(", dist_cols_sql, ") <= ", max_dist,
      con = con
    )

    uniq <- uniq |>
      filter(dbplyr::sql(sql_expr)) |>
      as_duckdb_tibble()
  }

  uniq |> compute()
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
