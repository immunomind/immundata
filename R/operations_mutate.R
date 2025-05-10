#' @title Modify or Add Columns to ImmunData Annotations
#'
#' @description
#' Applies transformations to the `$annotations` table within an `ImmunData`
#' object, similar to `dplyr::mutate`. It allows adding new columns or modifying
#' existing non-schema columns using standard `dplyr` expressions. Additionally,
#' it can add new columns based on sequence comparisons (exact match, regular
#' expression matching, or distance calculation) against specified patterns.
#'
#' @details
#' The function operates in two main steps:
#' 1.  **Standard Mutations (`...`)**: Applies the standard `dplyr::mutate`-style
#'     expressions provided in `...` to the `$annotations` table. You can create
#'     new columns or modify existing ones, but you *cannot* modify columns
#'     defined in the core `ImmunData` schema (e.g., `receptor_id`, `cell_id`).
#'     An error will occur if you attempt to do so.
#' 2.  **Sequence-based Annotations (`seq_options`)**: If `seq_options` is provided,
#'     the function calculates sequence similarities or distances and adds corresponding
#'     new columns to the `$annotations` table.
#'     * `method = "exact"`: Adds boolean columns (TRUE/FALSE) indicating whether the
#'         `query_col` value exactly matches each `pattern`. Column names are generated
#'         using a prefix (e.g., `sim_exact_`) and the pattern or its index.
#'     * `method = "regex"`: Uses `annotate_tbl_regex` to add columns indicating
#'         matches for each regular expression pattern against the `query_col`. The
#'         exact nature of the added columns depends on `annotate_tbl_regex` (e.g.,
#'         boolean flags or captured groups).
#'     * `method = "lev"` or `method = "hamm"`: Uses `annotate_tbl_distance` to
#'         calculate Levenshtein or Hamming distances between the `query_col` and
#'         each `pattern`, adding columns containing these numeric distances.
#'         `max_dist` is ignored in this context (internally treated as `NA`) as
#'         all distances are calculated and added, not used for filtering.
#'     * The naming of the new sequence-based columns depends on the `name_type`
#'         option within `seq_options` and internal helper functions like
#'         `make_pattern_columns`. Prefixes like `sim_exact_`, `sim_regex_`,
#'         `dist_lev_`, `dist_hamm_` are typically used based on the schema.
#'
#' The `$repertoires` table, if present in the input `idata`, is copied to the
#' output object without modification. This function only affects the `$annotations`
#' table.
#'
#' @param idata,.data An `ImmunData` object.
#' @param ... `dplyr::mutate`-style named expressions (e.g., `new_col = existing_col * 2`,
#'   `category = ifelse(value > 10, "high", "low")`). These are applied first.
#'   **Important**: You cannot use names for new or modified columns that conflict
#'   with the core `ImmunData` schema columns (retrieved via `imd_schema()`).
#' @param seq_options Optional named list specifying sequence-based annotation options.
#'   Use [make_seq_options()] for convenient creation. See `filter_immundata`
#'   documentation (`?filter_immundata`) or the details section here for the list
#'   structure (`query_col`, `patterns`, `method`, `name_type`). `max_dist` is
#'   ignored for mutation. If `NULL` (the default), no sequence-based columns are added.
#'
#' @return A *new* `ImmunData` object with the `$annotations` table modified according
#'   to the provided expressions and `seq_options`. The `$repertoires` table (if present)
#'   is carried over unchanged from the input `idata`.
#'
#' @seealso [dplyr::mutate()], [make_seq_options()], [filter_immundata()], [ImmunData],
#'   `vignette("immundata-classes", package = "immunarch")` (replace with actual package name if different)
#'
#' @examples
#' # Basic setup (assuming idata_test is a valid ImmunData object)
#' # print(idata_test)
#'
#' \dontrun{
#' # Example 1: Add a simple derived column
#' idata_mut1 <- mutate(idata_test, V_family = substr(V_gene, 1, 5))
#' print(idata_mut1$annotations)
#'
#' # Example 2: Add multiple columns and modify one (if 'custom_score' exists)
#' # Note: Avoid modifying core schema columns like 'V_gene' itself.
#' idata_mut2 <- mutate(idata_test,
#'   V_basic = gsub("-.*", "", V_gene),
#'   J_len = nchar(J_gene),
#'   custom_score = custom_score * 1.1
#' ) # Fails if custom_score doesn't exist
#' print(idata_mut2$annotations)
#'
#' # Example 3: Add boolean columns for exact CDR3 matches
#' cdr3_patterns <- c("CARGLGLVFYGMDVW", "CARDNRGAVAGVFGEAFYW")
#' seq_opts_exact <- make_seq_options(
#'   query_col = "CDR3_aa",
#'   patterns = cdr3_patterns,
#'   method = "exact",
#'   name_type = "pattern"
#' ) # Name cols by pattern
#' idata_mut_exact <- mutate(idata_test, seq_options = seq_opts_exact)
#' # Look for new columns like 'sim_exact_CARGLGLVFYGMDVW'
#' print(idata_mut_exact$annotations)
#'
#' # Example 4: Add Levenshtein distance columns for a CDR3 pattern
#' seq_opts_lev <- make_seq_options(
#'   query_col = "CDR3_aa",
#'   patterns = "CARGLGLVFYGMDVW",
#'   method = "lev",
#'   name_type = "index"
#' ) # Name col like 'dist_lev_1'
#' idata_mut_lev <- mutate(idata_test, seq_options = seq_opts_lev)
#' # Look for new column 'dist_lev_1' (or similar based on schema)
#' print(idata_mut_lev$annotations)
#'
#' # Example 5: Combine standard mutation and sequence annotation
#' seq_opts_regex <- make_seq_options(
#'   query_col = "V_gene",
#'   patterns = c(ighv1 = "^IGHV1-", ighv3 = "^IGHV3-"),
#'   method = "regex",
#'   name_type = "pattern"
#' )
#' idata_mut_combo <- mutate(idata_test,
#'   chain_upper = toupper(chain),
#'   seq_options = seq_opts_regex
#' )
#' # Look for 'chain_upper' and regex match columns (e.g., 'sim_regex_ighv1')
#' print(idata_mut_combo)
#' }
#'
#' @concept Mutation
#' @export
mutate_immundata <- function(idata,
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


#' @rdname mutate_immundata
#' @exportS3Method dplyr::mutate
mutate.ImmunData <- function(.data, ..., seq_options = NULL) {
  mutate_immundata(idata = .data, ..., seq_options = seq_options)
}
