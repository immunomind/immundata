#' @title Keep selected rows or receptors in ImmunData
#'
#' @description
#' Use `filter()` to keep selected rows in an [ImmunData] object. For example,
#' you can keep rows from one response group, rows using a selected V gene, or
#' receptors containing a CDR3 sequence similar to a reference sequence.
#'
#' The function returns a new [ImmunData] object. The original object is not
#' changed.
#'
#' This function is a direct implementation of [dplyr::filter]. Alternative
#' function name is `filter_immundata`.
#'
#' Use `filter_barcodes()` to keep selected cell barcodes and
#' `filter_receptors()` to keep selected receptor identifiers.
#'
#' @details
#' You can filter an [ImmunData] object in three ways:
#'
#' * Supply conditions in `...` to filter using annotation columns. Refer to
#'   columns directly by name. For example, `Response == "FR"` keeps rows from
#'   the `FR` response group.
#' * Supply `seq_options`, created with [make_seq_options()], to find receptors
#'   containing a sequence that matches one or more reference sequences or
#'   patterns.
#' * Use `filter_barcodes()` or `filter_receptors()` when you already have the
#'   identifiers that you want to keep.
#'
#' Conditions in `...` are applied before sequence matching. Sequence matching
#' then identifies receptors from the remaining rows. When one chain matches,
#' all remaining chains belonging to the same receptor are kept. A chain removed
#' by a condition in `...` is not added back by sequence matching.
#'
#' Sequence matching methods are:
#'
#' * `"exact"`: the sequence must be identical to one of the references.
#' * `"regex"`: the sequence must match a regular-expression pattern. This is
#'   an advanced option for matching text patterns.
#' * `"lev"`: the Levenshtein distance counts the substitutions, insertions, or
#'   deletions needed to change one sequence into the other.
#' * `"hamm"`: the Hamming distance counts different positions between
#'   sequences of the same length. Sequences of different lengths do not match.
#'
#' For `"lev"` and `"hamm"`, provide `max_dist`. A sequence is accepted when
#' its distance from at least one reference is less than or equal to this value.
#' A distance of `0` means an exact match, and smaller values mean more similar
#' sequences.
#'
#' By default, existing repertoire summaries are recalculated from the filtered
#' data. Existing strata are also rebuilt, and their labels are retained. Set
#' `keep_repertoires = FALSE` to return an object without repertoire or strata
#' summaries.
#'
#' @param idata,.data An [ImmunData] object.
#' @param ... One or more conditions used to keep rows. Refer to annotation
#'   columns directly by name. Multiple conditions are combined with `&`.
#'   Conditions are applied before sequence matching.
#' @param .by,.preserve Accepted for compatibility with [dplyr::filter()], but
#'   currently not used for [ImmunData] objects.
#' @param seq_options Options for matching sequences with reference sequences or
#'   patterns. Create these options with [make_seq_options()]. If `NULL`, the
#'   default, no sequence matching is performed.
#' @param keep_repertoires If `TRUE`, the default, existing repertoire and strata
#'   summaries are recalculated from the filtered data. If `FALSE`, the returned
#'   object does not contain these summaries.
#' @param barcodes A character, integer, or numeric vector of cell barcodes to
#'   keep with `filter_barcodes()`.
#' @param receptors A character, integer, or numeric vector of receptor
#'   identifiers to keep with `filter_receptors()`.
#'
#' @return A new [ImmunData] object containing the selected rows and receptors.
#' If requested, repertoire and strata summaries are recalculated for the
#' selected data.
#'
#' @seealso [dplyr::filter()], [make_seq_options()], [mutate_immundata()],
#'   [agg_repertoires()], [ImmunData]
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
#' # Keep rows from one response group
#' fr_response <- idata |>
#'   filter(Response == "FR")
#'
#' fr_response |>
#'   collect() |>
#'   summarise(
#'     n_rows = n(),
#'     n_receptors = n_distinct(imd_receptor_id)
#'   )
#' # Expected result:
#' #   n_rows n_receptors
#' #      955         871
#'
#' # Keep receptors containing one reference CDR3 sequence
#' reference_cdr3 <- "ASFPVLSPYNEQF"
#'
#' exact_match <- idata |>
#'   filter(
#'     seq_options = make_seq_options(
#'       query_col = "cdr3_aa",
#'       patterns = reference_cdr3,
#'       method = "exact"
#'     )
#'   )
#'
#' exact_match |>
#'   collect() |>
#'   select(cdr3_aa, v_call, Response)
#' # Expected result:
#' #   cdr3_aa       v_call    Response
#' #   ASFPVLSPYNEQF TRBV28*01 FR
#'
#' # Keep receptors within four sequence changes of the reference
#' similar_sequences <- idata |>
#'   filter(
#'     seq_options = make_seq_options(
#'       query_col = "cdr3_aa",
#'       patterns = reference_cdr3,
#'       method = "lev",
#'       max_dist = 4
#'     )
#'   )
#'
#' similar_sequences |>
#'   collect() |>
#'   distinct(cdr3_aa) |>
#'   arrange(cdr3_aa)
#' # Expected result:
#' #   cdr3_aa
#' #   ASFPVLSPYNEQF
#' #   ASSPDSPSYNEQF
#' #   ASSPGLAAYNEQF
#' #   ASSPTLYNEQF
#'
#' # Keep two selected cell barcodes
#' selected_barcodes <- c("S1_1", "S1_2")
#'
#' selected_cells <- idata |>
#'   filter_barcodes(selected_barcodes)
#'
#' selected_cells |>
#'   collect() |>
#'   distinct(imd_barcode)
#' # Expected result:
#' #   imd_barcode
#' #   S1_1
#' #   S1_2
#'
#' # The same approach can keep selected receptor identifiers
#' selected_receptors <- idata |>
#'   collect() |>
#'   distinct(imd_receptor_id) |>
#'   slice_head(n = 2) |>
#'   pull(imd_receptor_id)
#'
#' selected_receptors_data <- idata |>
#'   filter_receptors(selected_receptors)
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
      filtered_universe <- new_annotations

      keep_ids <- filtered_universe |>
        filter(!!col_sym %in% seq_options$patterns) |>
        select(all_of(receptor_id)) |>
        distinct()

      new_annotations <- filtered_universe |>
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
    # TODO: Refactor, but I'm not sure how to do it properly. Simply split to separate functions <compute distance> + <filter immundata>?
    # TODO: looks like a case for <move_annotations> from receptors to annotations
    if (seq_options$method != "exact") {
      keep_ids <- new_annotations |>
        semi_join(distance_data, by = seq_options$query_col) |>
        select(all_of(receptor_id)) |>
        distinct()

      new_annotations <- new_annotations |>
        semi_join(keep_ids, by = receptor_id)
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

  keep_repertoires <- keep_repertoires && !is.null(idata$schema_repertoire)
  if (!keep_repertoires) {
    new_annotations <- drop_repertoire_state(new_annotations)
  }

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    provenance = get_provenance(idata)
  )

  if (keep_repertoires) {
    rebuild_repertoire_and_strata(new_idata, idata)
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

  keep_repertoires <- keep_repertoires && !is.null(idata$schema_repertoire)
  if (!keep_repertoires) {
    new_annotations <- drop_repertoire_state(new_annotations)
  }

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    provenance = get_provenance(idata)
  )

  if (keep_repertoires) {
    rebuild_repertoire_and_strata(new_idata, idata)
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

  keep_repertoires <- keep_repertoires && !is.null(idata$schema_repertoire)
  if (!keep_repertoires) {
    new_annotations <- drop_repertoire_state(new_annotations)
  }

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    provenance = get_provenance(idata)
  )

  if (keep_repertoires) {
    rebuild_repertoire_and_strata(new_idata, idata)
  } else {
    new_idata
  }
}
