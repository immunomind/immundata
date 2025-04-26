#' @exportS3Method dplyr::mutate
mutate.ImmunData <- function(idata,
                             ...,
                             seq_options = NULL) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_list(seq_options, null.ok = TRUE)

  dots <- rlang::enquos(..., .named = TRUE) # keep names exactly as passed
  bad <- names(dots)[names(dots) %in% imd_schema()]

  if (length(bad)) {
    cli::cli_abort(
      "You cannot create or overwrite system columns. Offending names: {.val {bad}}"
    )
  }

  # Run "basic" mutate first
  new_annotations <- idata$annotations
  if (length(dots) > 0) {
    new_annotations <- new_annotations |> mutate(!!!dots)
  }

  receptor_id <- imd_schema("receptor")

  # Run the sequence-based mutations
  if (!is.null(seq_options)) {
    seq_options <- check_seq_options(seq_options, mode = "mutate")

    col_sym <- rlang::sym(seq_options$query_col)

    #
    # Exact
    #
    if (seq_options$method == "exact") {
      dist_cols <- make_pattern_columns(
        patterns = seq_options$patterns,
        col_prefix = imd_schema("sim_exact"),
        name_type = seq_options$name_type
      )

      for (p_index in seq_along(seq_options$patterns)) {
        p_seq <- seq_options$patterns[p_index]
        new_annotations <- new_annotations |>
          mutate(!!rlang::sym(dist_cols[p_index]) := !!col_sym == p_seq)
      }
    } else {
      #
      # Regex
      #
      if (seq_options$method == "regex") {
        distance_data <- annotate_tbl_regex(
          new_annotations |> select(!!col_sym),
          query_col = seq_options$query_col,
          patterns = seq_options$patterns,
          filter_out = FALSE,
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
          max_dist = NA,
          name_type = seq_options$name_type
        )
      }

      new_annotations <- new_annotations |>
        left_join(
          distance_data,
          by = seq_options$query_col
        )
    }
  }

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    repertoires = idata$repertoires
  )

  new_idata
}
