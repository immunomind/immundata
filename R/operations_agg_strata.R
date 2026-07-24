#' @title Aggregate repertoires into strata
#'
#' @description
#' Creates a strata layer above repertoires by grouping `idata$repertoires`
#' with user-selected metadata columns.
#'
#' This enables a hierarchy of
#' `chains -> barcodes -> receptors -> repertoires -> strata`.
#'
#' @param idata An `ImmunData` object with repertoire aggregation already
#'   available (run [agg_repertoires()] first).
#' @param schema Character vector of columns in `idata$repertoires` used to define
#'   strata.
#' @param strata_name_prefix Character(1). Prefix for automatic strata labels in
#'   `strata_name`. Default: `"Strata"`.
#'
#' @return A new `ImmunData` object where:
#' * `$repertoires` includes `imd_strata_id` and `strata_name`;
#' * `$annotations` includes `imd_strata_id` (joined by `imd_repertoire_id`).
#'
#' `strata_name` is stored only in `$repertoires` and is not copied into
#' `$annotations`.
#'
#' @details
#' Strata are derived from the current repertoire definition. Calling
#' `agg_strata()` on an already stratified object replaces the previous strata
#' mapping, and the generated `imd_strata_id` values should be treated as
#' internal identifiers rather than stable identifiers.
#'
#' Calling [agg_repertoires()] after `agg_strata()` intentionally rebuilds the
#' repertoire identifiers and statistics. Because the previous strata mapping
#' is tied to the old repertoire state, `imd_strata_id`, `strata_name`, and the
#' strata schema are removed rather than carried forward. To retain a strata
#' layer after redefining or recomputing repertoires, call `agg_strata()` again
#' with the desired schema. The same rule applies when another operation
#' re-aggregates repertoires internally.
#'
#' @seealso [agg_repertoires()], [rename_strata()], [ImmunData]
#'
#' @concept aggregation
#' @export
agg_strata <- function(idata, schema, strata_name_prefix = "Strata") {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(schema, min.len = 1, unique = TRUE, any.missing = FALSE)
  checkmate::assert_string(strata_name_prefix, min.chars = 1)

  if (is.null(idata$repertoires) || is.null(idata$schema_repertoire)) {
    cli::cli_abort(
      "Repertoire aggregation is required for {.fn agg_strata}. Run {.fn agg_repertoires} first."
    )
  }

  repertoire_col <- imd_schema("repertoire")
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  if (!(repertoire_col %in% colnames(idata$repertoires))) {
    cli::cli_abort(
      "Required column {.field {repertoire_col}} is missing in {.field idata$repertoires}."
    )
  }
  if (!(repertoire_col %in% colnames(idata$annotations))) {
    cli::cli_abort(
      "Required column {.field {repertoire_col}} is missing in {.field idata$annotations}."
    )
  }

  rep_tbl_clean <- idata$repertoires |>
    select(-any_of(c(strata_col, strata_name_col)))

  missing_schema_repertoires <- setdiff(schema, colnames(rep_tbl_clean))
  if (length(missing_schema_repertoires) > 0) {
    cli::cli_abort(
      "Column(s) [{missing_schema_repertoires}] specified in {.arg schema} are not found in {.field idata$repertoires}."
    )
  }

  strata_defs <- rep_tbl_clean |>
    select(all_of(schema)) |>
    distinct() |>
    arrange(!!!rlang::syms(schema)) |>
    mutate(
      {{ strata_col }} := row_number(),
      {{ strata_name_col }} := paste0(strata_name_prefix, .data[[strata_col]])
    ) |>
    select(all_of(c(strata_col, strata_name_col, schema)))

  rep_tbl_stratified <- rep_tbl_clean |>
    mutate(.__row_id = row_number()) |>
    left_join(
      strata_defs |> select(all_of(c(schema, strata_col, strata_name_col))),
      by = schema
    ) |>
    arrange(.__row_id) |>
    select(-all_of(".__row_id"))

  rep_to_strata <- rep_tbl_stratified |>
    select(all_of(c(repertoire_col, strata_col))) |>
    distinct()

  annotations_stratified <- idata$annotations |>
    select(-any_of(strata_col)) |>
    left_join(
      duckplyr::as_duckdb_tibble(rep_to_strata),
      by = repertoire_col
    )

  ImmunData$new(
    schema = idata$schema_receptor,
    annotations = annotations_stratified,
    repertoires = rep_tbl_stratified,
    strata = duckplyr::as_duckdb_tibble(strata_defs),
    provenance = imd_get_provenance(idata)
  )
}


#' @title Rename strata labels
#'
#' @description
#' Renames `strata_name` values for existing strata (`imd_strata_id`) in
#' `idata$repertoires`.
#'
#' This function updates only repertoire-level metadata; annotations remain
#' unchanged and keep only `imd_strata_id`.
#'
#' @param idata An `ImmunData` object with strata already created by
#'   [agg_strata()].
#' @param names Mapping from `imd_strata_id` to `strata_name`. Supported forms:
#'   * named character vector, where names are strata IDs;
#'   * data frame with columns `imd_strata_id` and `strata_name`.
#' @param unnamed What to do with strata IDs not covered by `names`:
#'   `"error"` (default), `"auto"`, or `"keep"`.
#' @param auto_prefix Prefix used when `unnamed = "auto"` (or when legacy
#'   objects miss `strata_name`). Default: `"Strata"`.
#'
#' @return A new `ImmunData` object with updated `strata_name` in
#'   `$repertoires`.
#'
#' @seealso [agg_strata()], [agg_repertoires()]
#'
#' @concept aggregation
#' @export
rename_strata <- function(idata, names, unnamed = c("error", "auto", "keep"), auto_prefix = "Strata") {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_string(auto_prefix, min.chars = 1)
  unnamed <- match.arg(unnamed)

  if (is.null(idata$repertoires) || is.null(idata$schema_repertoire)) {
    cli::cli_abort(
      "Repertoire aggregation is required for {.fn rename_strata}. Run {.fn agg_repertoires} and {.fn agg_strata} first."
    )
  }

  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  rep_tbl <- idata$repertoires

  if (!(strata_col %in% colnames(rep_tbl))) {
    cli::cli_abort(
      "Required column {.field {strata_col}} is missing in {.field idata$repertoires}. Run {.fn agg_strata} first."
    )
  }

  if (!(strata_name_col %in% colnames(rep_tbl))) {
    rep_tbl[[strata_name_col]] <- paste0(auto_prefix, rep_tbl[[strata_col]])
  }

  if (checkmate::test_character(names, min.len = 1, names = "named", any.missing = FALSE)) {
    map_tbl <- data.frame(
      id = base::names(names),
      label = unname(names),
      stringsAsFactors = FALSE
    )
    names(map_tbl) <- c(strata_col, strata_name_col)
  } else if (checkmate::test_data_frame(names)) {
    map_tbl <- names
    required_cols <- c(strata_col, strata_name_col)
    missing_cols <- setdiff(required_cols, colnames(map_tbl))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "Missing required column(s) in {.arg names}: [{missing_cols}]."
      )
    }
    map_tbl <- map_tbl[, required_cols, drop = FALSE]
  } else {
    cli::cli_abort(
      "{.arg names} must be either a named character vector or a data frame with columns [{strata_col}, {strata_name_col}]."
    )
  }

  map_tbl[[strata_col]] <- suppressWarnings(as.integer(as.character(map_tbl[[strata_col]])))
  if (any(is.na(map_tbl[[strata_col]]))) {
    cli::cli_abort("All strata IDs in {.arg names} must be coercible to integer.")
  }

  map_tbl[[strata_name_col]] <- trimws(as.character(map_tbl[[strata_name_col]]))
  if (any(is.na(map_tbl[[strata_name_col]]) | map_tbl[[strata_name_col]] == "")) {
    cli::cli_abort("All strata labels in {.arg names} must be non-empty strings.")
  }

  if (anyDuplicated(map_tbl[[strata_col]]) > 0) {
    cli::cli_abort("Found duplicated strata IDs in {.arg names}.")
  }
  if (anyDuplicated(map_tbl[[strata_name_col]]) > 0) {
    cli::cli_abort("Found duplicated strata labels in {.arg names}.")
  }

  strata_ids <- unique(rep_tbl[[strata_col]])
  unknown_ids <- setdiff(unique(map_tbl[[strata_col]]), strata_ids)
  if (length(unknown_ids) > 0) {
    cli::cli_abort(
      "Unknown strata ID(s) in {.arg names}: [{unknown_ids}]."
    )
  }

  idx <- match(rep_tbl[[strata_col]], map_tbl[[strata_col]])
  has_map <- !is.na(idx)
  rep_tbl[[strata_name_col]][has_map] <- map_tbl[[strata_name_col]][idx[has_map]]

  unmapped_ids <- unique(rep_tbl[[strata_col]][!has_map])
  if (length(unmapped_ids) > 0) {
    if (identical(unnamed, "error")) {
      cli::cli_abort(
        "Missing names for strata ID(s): [{unmapped_ids}]. Provide a complete mapping or use {.code unnamed = 'auto'} / {.code unnamed = 'keep'}."
      )
    } else if (identical(unnamed, "auto")) {
      rep_tbl[[strata_name_col]][!has_map] <- paste0(auto_prefix, rep_tbl[[strata_col]][!has_map])
    }
  }

  uniq_labels <- unique(rep_tbl[c(strata_col, strata_name_col)])
  if (any(is.na(uniq_labels[[strata_name_col]]) | trimws(uniq_labels[[strata_name_col]]) == "")) {
    cli::cli_abort("Resulting strata labels contain missing or empty values.")
  }
  if (anyDuplicated(uniq_labels[[strata_name_col]]) > 0) {
    cli::cli_abort("Resulting strata labels are not unique across strata.")
  }

  strata_table <- rep_tbl |>
    select(all_of(c(strata_col, strata_name_col, idata$schema_strata))) |>
    distinct() |>
    duckplyr::as_duckdb_tibble()

  ImmunData$new(
    schema = idata$schema_receptor,
    annotations = idata$annotations,
    repertoires = rep_tbl,
    strata = strata_table,
    provenance = imd_get_provenance(idata)
  )
}
