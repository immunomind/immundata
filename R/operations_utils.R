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
#' @concept utils
#' @export
make_receptor_schema <- function(features, chains = NULL) {
  checkmate::assert_character(features, min.len = 1, any.missing = FALSE)
  checkmate::assert_character(
    chains,
    min.len = 1,
    max.len = 2,
    any.missing = FALSE,
    null.ok = TRUE
  )

  list(features = features, chains = chains)
}


#' @rdname make_receptor_schema
#' @export
assert_receptor_schema <- function(schema) {
  # TODO: globals.R with schema list

  checkmate::assert(
    checkmate::check_character(schema, min.len = 1),
    checkmate::check_list(schema, len = 2, null.ok = FALSE) &&
      checkmate::check_names(names(schema), must.include = c("features", "chains"))
  )

  receptor_chains <- imd_receptor_chains(schema)

  if (!is.null(receptor_chains)) {
    # Validate chain syntax rules
    if (length(receptor_chains) > 2) {
      cli::cli_abort("Schema can have at most 2 chain elements. Found {length(receptor_chains)}: [{paste(receptor_chains, collapse=', ')}]")
    }

    if (length(receptor_chains) >= 1) {
      # Check first chain doesn't contain pipe
      if (grepl("\\|", receptor_chains[1])) {
        cli::cli_abort("The first chain in the schema cannot contain '|' character. Found: '{receptor_chains[1]}'. The OR syntax is only allowed in the second chain.")
      }
    }

    if (length(receptor_chains) == 2) {
      # Check if second chain contains OR syntax (|)
      if (grepl("\\|", receptor_chains[2])) {
        # Split and validate the alternatives
        relaxed_chain_alternatives <- trimws(unlist(strsplit(receptor_chains[2], "\\|")))

        # Validate the alternatives
        if (length(relaxed_chain_alternatives) != 2) {
          cli::cli_abort("Relaxed pairing syntax requires exactly 2 alternatives separated by '|'. Found {length(relaxed_chain_alternatives)} in '{receptor_chains[2]}'")
        }

        # Check for empty alternatives
        if (any(relaxed_chain_alternatives == "")) {
          cli::cli_abort("Empty chain name found in '{receptor_chains[2]}'. Both alternatives must be valid chain names.")
        }

        # Check for duplicate alternatives
        if (length(unique(relaxed_chain_alternatives)) != length(relaxed_chain_alternatives)) {
          cli::cli_abort("Duplicate chain names found in '{receptor_chains[2]}'. Alternatives must be different.")
        }

        # Check that alternatives are different from the required chain
        if (receptor_chains[1] %in% relaxed_chain_alternatives) {
          cli::cli_abort("The required chain '{receptor_chains[1]}' cannot also be an alternative in '{receptor_chains[2]}'")
        }
      } else {
        # Strict pairing - check for accidental spaces or typos
        if (grepl("[\\s,;]", receptor_chains[2])) {
          cli::cli_warn("Found potential separator characters in '{receptor_chains[2]}'. For relaxed pairing, use the pipe character '|' to separate alternatives (e.g., 'IGL|IGK')")
        }
      }
    }
  }

  TRUE
}


#' @rdname make_receptor_schema
#' @export
test_receptor_schema <- function(schema) {
  checkmate::test_character(schema, min.len = 1) || (
    checkmate::test_list(schema, len = 2, null.ok = FALSE) &&
      checkmate::test_subset(names(schema), c("features", "chains"), empty.ok = FALSE)
  )
}
