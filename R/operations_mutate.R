#' @title Add or change annotation columns in ImmunData
#'
#' @description
#' Use `mutate()` to add information to each row of an [ImmunData] object. For
#' example, you can calculate CDR3 length, mark sequences of interest, or compare
#' receptor sequences with reference sequences.
#'
#' The function returns a new [ImmunData] object. The original object is not
#' changed.
#'
#' This function is a direct implementation of [dplyr::mutate]. Alternative
#' function name is `mutate_immundata`.
#'
#' @details
#' You can use `mutate()` in three ways:
#'
#' * Supply named calculations in `...` to create annotation columns from
#'   existing data. For example, `cmv_specific = cdr3_aa %in% cmv_cdr3s` adds a
#'   column containing `TRUE` or `FALSE` for each row.
#' * Supply `.by` to perform calculations separately for temporary groups. The
#'   number of rows does not change. A group statistic is repeated for all rows
#'   in that group.
#' * Supply `seq_options`, created with [make_seq_options()], to compare a
#'   sequence column with one or more reference sequences or patterns. One result
#'   column is added for each reference.
#'
#' Named calculations in `...` are performed before sequence comparisons.
#'
#' Most grouped calculations are translated directly to DuckDB. Some group
#' statistics, such as `n_distinct()`, are not available as DuckDB window
#' calculations when a large dataset must stay on disk. In that case, `mutate()`
#' automatically calculates one summary row per group and joins the values back
#' to the annotation rows. This remains lazy and does not load the full dataset
#' into R memory.
#'
#' The automatic fallback works when every calculation in the call produces one
#' value per group. If a call combines a row-level calculation with a group
#' statistic that needs the fallback, use two `mutate()` calls. Also use a second
#' call when a later calculation refers to a group statistic created by the
#' fallback. See the examples below.
#'
#' Columns used to identify receptors or repertoires, and identifiers managed by
#' `ImmunData`, are protected. This prevents accidental changes that would make
#' the object inconsistent. You can add new columns and change other annotation
#' columns.
#'
#' Sequence comparison methods are:
#'
#' * `"exact"`: `TRUE` when the sequence is identical to the reference.
#' * `"regex"`: `TRUE` when the sequence matches a regular-expression pattern.
#'   This is an advanced option for matching text patterns.
#' * `"lev"`: the number of substitutions, insertions, or deletions needed to
#'   change one sequence into the other.
#' * `"hamm"`: the number of different positions between sequences of the same
#'   length. Sequences with different lengths receive `NA`.
#'
#' For the distance methods, `0` means an exact match and smaller values mean
#' more similar sequences. With `name_type = "index"`, the result columns have
#' short names such as `imd_sim_exact_1` or `imd_sim_lev_1`. With
#' `name_type = "pattern"`, each column name includes its reference pattern.
#'
#' `max_dist` is used by [filter_immundata()] but has no effect here because
#' `mutate()` reports every calculated distance.
#'
#' Existing repertoire and strata summaries are carried to the new object
#' without modification.
#'
#' @param idata,.data An [ImmunData] object.
#' @param ... One or more named calculations in the form
#'   `new_column = calculation`. Refer to existing columns directly by name. You
#'   can add new annotation columns or change columns that are not protected.
#' @param .by Optional columns used to form temporary groups for this operation.
#'   For example, `.by = Response` calculates separately for each response, and
#'   `.by = c(Response, imd_group_id)` uses each response and receptor-cluster
#'   combination. The grouping applies only to this `mutate()` call.
#' @param seq_options Options for comparing sequences with reference sequences or
#'   patterns. Create these options with [make_seq_options()]. If `NULL`, the
#'   default, no sequence comparisons are performed.
#'
#' @return A new [ImmunData] object containing the added or changed annotation
#'   columns. Existing repertoire and strata summaries are preserved.
#'
#' @seealso [dplyr::mutate()], [make_seq_options()], [filter_immundata()],
#'   [annotate_receptors()], [agg_repertoires()], [ImmunData]
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Load data included with immundata
#' idata <- get_test_idata()
#'
#' # Add the length of each CDR3 amino acid sequence
#' idata_with_length <- idata |>
#'   mutate(cdr3_length = dd$length(cdr3_aa))
#'
#' idata_with_length |>
#'   collect() |>
#'   select(cdr3_aa, cdr3_length) |>
#'   slice_head(n = 3)
#' # Expected result:
#' #   cdr3_aa       cdr3_length
#' #   ASFPVLSPYNEQF          13
#' #   ASRAGAGTGELF           12
#' #   ASSPGQGLDTQY           12
#'
#' # Compare CDR3 sequences with one reference sequence
#' reference_cdr3 <- "ASFPVLSPYNEQF"
#'
#' idata_with_matches <- idata |>
#'   mutate(
#'     seq_options = make_seq_options(
#'       query_col = "cdr3_aa",
#'       patterns = reference_cdr3,
#'       method = "exact"
#'     )
#'   )
#'
#' idata_with_matches |>
#'   collect() |>
#'   count(imd_sim_exact_1)
#' # Expected result:
#' #   imd_sim_exact_1     n
#' #   FALSE            1901
#' #   TRUE                1
#'
#' # Calculate Levenshtein distance from the reference sequence
#' idata_with_distance <- idata |>
#'   mutate(
#'     seq_options = make_seq_options(
#'       query_col = "cdr3_aa",
#'       patterns = reference_cdr3,
#'       method = "lev"
#'     )
#'   )
#'
#' idata_with_distance |>
#'   collect() |>
#'   select(cdr3_aa, imd_sim_lev_1) |>
#'   arrange(imd_sim_lev_1, cdr3_aa) |>
#'   slice_head(n = 3)
#' # Expected result:
#' #   cdr3_aa       imd_sim_lev_1
#' #   ASFPVLSPYNEQF             0
#' #   ASSPDSPSYNEQF             4
#' #   ASSPGLAAYNEQF             4
#'
#' # Mark selected sequences
#' cmv_cdr3s <- c(
#'   "ASFPVLSPYNEQF",
#'   "ASRAGAGTGELF"
#' )
#'
#' marked_sequences <- idata |>
#'   mutate(
#'     cmv_specific = cdr3_aa %in% cmv_cdr3s
#'   )
#'
#' marked_sequences |>
#'   collect() |>
#'   count(cmv_specific)
#' # Expected result:
#' #   cmv_specific     n
#' #   FALSE         1900
#' #   TRUE             2
#'
#' # Mark selected receptor identities
#' cmv_hits <- tibble(
#'   imd_receptor_id = c(1L, 105L),
#'   cmv_specific = TRUE
#' )
#'
#' marked_receptors <- idata |>
#'   annotate_receptors(cmv_hits) |>
#'   mutate(
#'     cmv_specific = coalesce(cmv_specific, FALSE)
#'   )
#'
#' marked_receptors |>
#'   collect() |>
#'   count(cmv_specific)
#' # Expected result:
#' #   cmv_specific     n
#' #   FALSE         1898
#' #   TRUE             4
#'
#' # Add response-level statistics to every annotation row
#' # `.by` means: calculate separately for each response.
#' response_stats <- idata |>
#'   mutate(
#'     response_n_rows = n(),
#'     response_n_receptors = n_distinct(imd_receptor_id),
#'     .by = Response
#'   )
#'
#' response_stats |>
#'   collect() |>
#'   distinct(Response, response_n_rows, response_n_receptors) |>
#'   arrange(Response)
#' # Expected result:
#' #   Response response_n_rows response_n_receptors
#' #   FR                   955                  871
#' #   PR                   947                  867
#'
#' # A grouped calculation can also produce a different value for every row.
#' response_centered <- idata |>
#'   mutate(
#'     centered_counts = counts - mean(counts, na.rm = TRUE),
#'     .by = Response
#'   )
#'
#' # Do not combine that row-level calculation with a statistic that needs the
#' # automatic summary fallback in the same call:
#' \dontrun{
#' idata |>
#'   mutate(
#'     centered_counts = counts - mean(counts, na.rm = TRUE),
#'     response_n_receptors = n_distinct(imd_receptor_id),
#'     .by = Response
#'   )
#' }
#'
#' # Use two mutate calls instead. The work remains lazy in DuckDB.
#' response_details <- idata |>
#'   mutate(
#'     centered_counts = counts - mean(counts, na.rm = TRUE),
#'     .by = Response
#'   ) |>
#'   mutate(
#'     response_n_receptors = n_distinct(imd_receptor_id),
#'     .by = Response
#'   )
#'
#' # Also use a second call when a new calculation uses a statistic created by
#' # the fallback.
#' response_details <- idata |>
#'   mutate(
#'     response_n_receptors = n_distinct(imd_receptor_id),
#'     .by = Response
#'   ) |>
#'   mutate(
#'     twice_response_n_receptors = response_n_receptors * 2
#'   )
#'
#' @concept mutation
#' @export
mutate_immundata <- function(idata,
                             ...,
                             .by = NULL,
                             seq_options = NULL) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_list(seq_options, null.ok = TRUE)

  dots <- rlang::enquos(..., .named = TRUE) # keep names exactly as passed
  by <- rlang::enquo(.by)
  assert_mutable_annotation_columns(idata, names(dots))

  sequence_annotation_cols <- NULL
  if (!is.null(seq_options)) {
    seq_options <- check_seq_options(seq_options, mode = "mutate")
    sequence_col_prefix <- switch(seq_options$method,
      exact = imd_schema("sim_exact"),
      regex = imd_schema("sim_regex"),
      lev = imd_schema("sim_lev"),
      hamm = imd_schema("sim_hamm")
    )
    sequence_annotation_cols <- make_pattern_columns(
      patterns = seq_options$patterns,
      col_prefix = sequence_col_prefix,
      name_type = seq_options$name_type
    )

    assert_mutable_annotation_columns(idata, sequence_annotation_cols)
  }

  # Run "basic" mutate first
  new_annotations <- idata$annotations
  if (length(dots) > 0) {
    new_annotations <- mutate_annotations_by(
      annotations = new_annotations,
      dots = dots,
      by = by
    )
  }

  receptor_id <- imd_schema("receptor")

  # Run the sequence-based mutations
  if (!is.null(seq_options)) {
    col_sym <- rlang::sym(seq_options$query_col)

    #
    # Exact
    #
    if (seq_options$method == "exact") {
      for (p_index in seq_along(seq_options$patterns)) {
        p_seq <- seq_options$patterns[p_index]
        new_annotations <- new_annotations |>
          mutate(!!rlang::sym(sequence_annotation_cols[p_index]) := !!col_sym == p_seq)
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

  clone_with_annotations(idata, new_annotations)
}


#' @rdname mutate_immundata
#' @exportS3Method dplyr::mutate
mutate.ImmunData <- function(.data, ..., .by = NULL, seq_options = NULL) {
  mutate_immundata(
    idata = .data,
    ...,
    .by = {{ .by }},
    seq_options = seq_options
  )
}


is_unsupported_duckplyr_window_error <- function(error) {
  parent <- error$parent

  inherits(error, "rlang_error") &&
    !is.null(parent) &&
    grepl(
      "stingy duckplyr frame",
      conditionMessage(error),
      fixed = TRUE
    ) &&
    grepl(
      "not supported in window functions",
      conditionMessage(parent),
      fixed = TRUE
    )
}

mutate_annotations_by <- function(annotations, dots, by) {
  mutated <- tryCatch(
    annotations |>
      mutate(!!!dots, .by = !!by),
    error = identity
  )

  if (!inherits(mutated, "error")) {
    return(mutated)
  }

  if (!is_unsupported_duckplyr_window_error(mutated) || rlang::quo_is_null(by)) {
    rlang::cnd_signal(mutated)
  }

  by_names <- names(annotations |> select(!!by))
  if (length(intersect(names(dots), by_names)) > 0) {
    rlang::cnd_signal(mutated)
  }

  stats <- tryCatch(
    annotations |>
      summarise(!!!dots, .by = !!by),
    error = identity
  )

  if (inherits(stats, "error")) {
    rlang::cnd_signal(mutated)
  }

  value_names <- setdiff(names(stats), by_names)
  desired_order <- base::union(names(annotations), value_names)

  annotations |>
    select(-any_of(value_names)) |>
    left_join(stats, by = by_names, na_matches = "na") |>
    select(all_of(desired_order))
}
