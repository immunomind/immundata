#' @title Group repertoires into biological strata
#'
#' @description
#' Use `agg_strata()` to place sample repertoires into biological comparison
#' groups, such as treatment arms, tissues, or disease groups.
#'
#' Use this function after [agg_repertoires()] when several repertoires should be
#' analysed as one group. A *stratum* contains every repertoire with the same
#' value, or the same combination of values, in `schema`.
#'
#' The unit being grouped is a whole repertoire. The function returns a new
#' [ImmunData] object. The original object is not changed.
#'
#' @param idata An [ImmunData] object with repertoires already defined. Use
#'   [agg_repertoires()] first if the object does not contain repertoires.
#' @param schema A non-empty character vector. One or more repertoire-level
#'   columns that define a stratum. For example, use `"Therapy"` for treatment
#'   arms or `c("Tissue", "Disease")` for each tissue and disease combination.
#'   The columns must be present in `idata$repertoires`.
#' @param prefix A non-empty character string. Prefix for the
#'   automatic stratum labels. The default, `"Strata"`, produces labels such as
#'   `"Strata1"` and `"Strata2"`. You can use [rename_strata()] to assign meaningful
#'   labels later instead.
#'
#' @return A new [ImmunData] object in which every repertoire belongs to one
#'   stratum. The `$strata` table lists the strata, their defining biological
#'   values, and their automatic labels. Repertoire definitions and summary
#'   statistics are preserved.
#'
#' @details
#' If `schema` contains several columns, a separate stratum is created for each
#' observed combination. For example, `c("Tissue", "Therapy")` can define
#' separate blood and tumour strata within each treatment arm.
#'
#' Calling `agg_strata()` again replaces the existing strata with groups defined
#' by the new `schema`.
#'
#' @section Identifiers and storage:
#'
#' `imd_strata_id` is an internal identifier and can change when strata are
#' rebuilt. It is added to the repertoire table and to the underlying chain
#' annotations. `strata_name` is stored only in the smaller repertoire and
#' strata tables.
#'
#' Calling [agg_repertoires()] again rebuilds the repertoires, so it removes the
#' existing strata. Call `agg_strata()` again after redefining repertoires.
#'
#' @seealso [agg_repertoires()], [rename_strata()], [ImmunData]
#'
#' @concept aggregation
#' @export
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Define sample repertoires using the biological metadata in the test data
#' idata <- get_test_idata() |>
#'   agg_repertoires(c("Response", "Therapy"))
#'
#' # Group the sample repertoires into treatment arms
#' treatment_groups <- idata |>
#'   agg_strata(schema = "Therapy")
#'
#' treatment_groups$repertoires |>
#'   select(Therapy, Response, imd_strata_id, strata_name) |>
#'   arrange(imd_strata_id)
#' # Expected result:
#' #   Therapy Response imd_strata_id strata_name
#' #   CAR-T   PR                   1 Strata1
#' #   ICI     FR                   2 Strata2
#'
#' # Each repertoire is now assigned to its treatment stratum. Any additional
#' # repertoire with the same Therapy value would receive the same stratum ID.
agg_strata <- function(idata, schema, prefix = "Strata") {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(schema, min.len = 1, unique = TRUE, any.missing = FALSE)
  checkmate::assert_string(prefix, min.chars = 1)

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
      {{ strata_name_col }} := paste0(prefix, .data[[strata_col]])
    ) |>
    select(all_of(c(strata_col, strata_name_col, schema)))

  row_id_col <- ".__row_id"

  rep_tbl_stratified <- rep_tbl_clean |>
    mutate(!!row_id_col := row_number()) |>
    left_join(
      strata_defs |> select(all_of(c(schema, strata_col, strata_name_col))),
      by = schema
    ) |>
    arrange(!!rlang::sym(row_id_col)) |>
    select(-all_of(row_id_col))

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
    provenance = get_provenance(idata)
  )
}


#' @title Give biological strata readable labels
#'
#' @description
#' Use `rename_strata()` to replace automatic stratum labels with names that are
#' clear in figures and result tables, such as `"Control"`, `"Treated"`, or
#' `"Tumour tissue"`.
#'
#' Use this function after [agg_strata()] when labels such as `"Strata1"` do not
#' describe the biological groups. The unit being changed is the stratum label.
#' Stratum membership and the repertoires, receptors, cells, and chains remain
#' unchanged.
#'
#' The function returns a new [ImmunData] object. The original object is not
#' changed.
#'
#' @param idata An [ImmunData] object with strata already created by
#'   [agg_strata()].
#' @param names A named character vector or a data frame. New labels matched to
#'   `imd_strata_id`. Supply either:
#'
#'   * a named character vector, such as
#'     `c("1" = "Control", "2" = "Treated")`; or
#'   * a data frame with columns `imd_strata_id` and `strata_name`.
#'
#'   Every new label must be non-empty and unique.
#' @param unnamed A character string. What to do when `names` does not include
#'   every stratum. The default, `"error"`, asks for a complete mapping. Use
#'   `"auto"` to generate labels for missing strata or `"keep"` to preserve
#'   their current labels.
#' @param auto_prefix A non-empty character string. Prefix used to generate
#'   labels when `unnamed = "auto"`. The default is `"Strata"`.
#'
#' @return A new [ImmunData] object with the requested labels in its `$strata`
#'   and `$repertoires` tables. All biological group assignments and repertoire
#'   summaries are preserved.
#'
#' @details
#' The names of a named character vector are the stratum IDs, not the current
#' labels. Inspect `idata$strata` to find the ID for each biological group.
#'
#' The mapping cannot contain unknown or repeated IDs, and the resulting labels
#' must be unique across strata.
#'
#' @section Storage details:
#'
#' The readable `strata_name` is stored in the repertoire and strata tables. The
#' underlying chain annotations keep only `imd_strata_id`, so renaming a stratum
#' does not rewrite or regroup chain-level data.
#'
#' @seealso [agg_strata()], [agg_repertoires()]
#'
#' @concept aggregation
#' @export
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Create treatment strata for the sample repertoires in the test data
#' treatment_groups <- get_test_idata() |>
#'   agg_repertoires(c("Response", "Therapy")) |>
#'   agg_strata(schema = "Therapy")
#'
#' # Replace automatic labels with names suitable for a figure
#' labeled_groups <- treatment_groups |>
#'   rename_strata(
#'     names = c("1" = "CAR-T arm", "2" = "ICI arm")
#'   )
#'
#' labeled_groups$strata |>
#'   select(Therapy, imd_strata_id, strata_name) |>
#'   arrange(imd_strata_id)
#' # Expected result:
#' #   Therapy imd_strata_id strata_name
#' #   CAR-T               1 CAR-T arm
#' #   ICI                 2 ICI arm
#'
#' # Only the labels changed. Each sample repertoire remains in the same
#' # treatment stratum.
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
    provenance = get_provenance(idata)
  )
}
