#' @title Add external information to ImmunData
#'
#' @description
#' Use the `annotate_*()` functions to add information stored in another data
#' frame to an [ImmunData] object. For example, you can add cell types from a
#' single-cell analysis, antigen labels for receptors, or clinical information
#' for samples.
#'
#' When matching identifiers are unique, the functions keep every row in
#' `idata`. When a row has no match in `annotations`, the new columns contain
#' `NA`. Each function returns a new [ImmunData] object. The original object is
#' not changed.
#'
#' @details
#' The functions differ in how they select the columns used for matching. The
#' rules for duplicate identifiers, column conflicts, and preserved summaries
#' are the same for all functions.
#'
#' @section Choose a function:
#'
#' Use the function that matches the type of information you want to add:
#'
#' * [annotate_barcodes()] matches cell or barcode identifiers.
#' * [annotate_receptors()] matches receptor identifiers. All rows belonging to
#'   a matched receptor receive the new information.
#' * [annotate_chains()] matches chain identifiers.
#' * `annotate()` matches any one or more columns that you specify in `by`.
#'
#' The first three functions select the correct `ImmunData` identifier for you.
#' `annotate_immundata()` is an alternative name for `annotate()`.
#'
#' @section How matching works:
#'
#' For `annotate_barcodes()`, `annotate_receptors()`, and `annotate_chains()`,
#' `annot_col` names the identifier column in `annotations`. For example,
#' `annot_col = "barcode"` matches the `barcode` column in `annotations` with
#' the barcode identifier in `idata`.
#'
#' For general matching, supply `by` in the form
#' `c(immundata_column = "annotation_column")`. For example,
#' `by = c(Response = "response_code")` matches the `Response` column in
#' `idata` with the `response_code` column in `annotations`. To match columns
#' with the same name, use a value such as `by = c(Response = "Response")`.
#' You can include more than one pair of columns in `by`.
#'
#' Columns from `annotations` that are not used for matching are added to the
#' result. Rows in `idata` without a match receive `NA`. Rows in `annotations`
#' without a match are ignored.
#'
#' @section Annotation identifiers must be unique:
#'
#' `annotations` must contain at most one row for each identifier, or each
#' combination of identifiers when matching several columns. For example, a
#' barcode annotation table must contain at most one row per barcode.
#'
#' The function does not check this rule because the annotation table may be
#' very large. If an identifier occurs several times, the corresponding rows in
#' `idata` are repeated. This can make receptor counts, proportions, and other
#' summaries incorrect.
#'
#' @section Existing annotation columns:
#'
#' By default, the function stops if a new annotation column has the same name
#' as a column already present in `idata`. This prevents accidental replacement.
#'
#' Use `conflicts = "replace"` to replace existing annotation columns. Columns
#' that define receptors, repertoires, strata, or other `ImmunData` state are
#' protected and cannot be replaced. The old column is removed before matching,
#' so rows without a new match receive `NA`.
#'
#' @section Repertoire and strata summaries:
#'
#' With the default `keep_repertoires = TRUE`, existing repertoire and strata
#' summaries are copied to the new object without recalculation. Use this option
#' when you are only adding information and the matching identifiers in
#' `annotations` are unique.
#'
#' Set `keep_repertoires = FALSE` when you plan to filter rows or define new
#' repertoires using the added information. This removes existing repertoire and
#' strata summaries and their derived columns. After annotation and filtering,
#' use [agg_repertoires()] to define the new repertoires.
#'
#' @section Very wide annotation tables:
#'
#' By default, the function stops when `annotations` contains 100 or more
#' columns. Adding a very wide table, such as a complete gene-expression matrix,
#' can be slow and require a large amount of memory. If you understand this cost,
#' set `remove_limit = TRUE` to allow the operation.
#'
#' @param idata An [ImmunData] object.
#' @param annotations A data frame containing the information to add. It must
#'   contain the columns used for matching and at most one row for each matching
#'   identifier or combination of identifiers.
#' @param by A named character vector describing how columns are matched. Names
#'   are columns in `idata`; values are the corresponding columns in
#'   `annotations`. For example, `c(Response = "response_code")`.
#' @param annot_col Name of the identifier column in `annotations`. For
#'   `annotate_receptors()` and `annotate_chains()`, the default is the standard
#'   `ImmunData` receptor or chain identifier. For `annotate_barcodes()`, the
#'   default `"<rownames>"` uses the row names of `annotations`. Supplying an
#'   explicit barcode column is usually clearer.
#' @param keep_repertoires Whether to preserve existing repertoire and strata
#'   summaries without recalculation. The default is `TRUE`. If `FALSE`, these
#'   summaries and their derived annotation columns are removed.
#' @param remove_limit Whether to allow an annotation table with 100 or more
#'   columns. The default is `FALSE`, which stops the operation for such tables.
#'   Set to `TRUE` only when the wide join is intentional.
#' @param conflicts How to handle new annotation columns whose names already
#'   exist in `idata`. `"error"`, the default, stops the operation. `"replace"`
#'   replaces existing columns that are not protected by `ImmunData`.
#'
#' @return A new [ImmunData] object containing the added annotation columns.
#' Existing repertoire and strata summaries are preserved when
#' `keep_repertoires = TRUE`.
#'
#' @seealso [dplyr::left_join()], [agg_repertoires()], [filter_immundata()],
#'   [mutate_immundata()], [ImmunData]
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
#' # Add cell types by matching barcode identifiers
#' cell_labels <- tibble(
#'   barcode = c("S1_1", "S1_2"),
#'   cell_type = c("CD8 T cell", "CD4 T cell")
#' )
#'
#' idata_with_cells <- idata |>
#'   annotate_barcodes(
#'     annotations = cell_labels,
#'     annot_col = "barcode"
#'   )
#'
#' idata_with_cells |>
#'   collect() |>
#'   filter(imd_barcode %in% c("S1_1", "S1_2", "S1_3")) |>
#'   select(imd_barcode, cell_type) |>
#'   arrange(imd_barcode)
#' # Expected result:
#' #   imd_barcode cell_type
#' #   S1_1        CD8 T cell
#' #   S1_2        CD4 T cell
#' #   S1_3        NA
#'
#' # Add antigen labels to selected receptors
#' receptor_labels <- tibble(
#'   receptor_id = c(738L, 1567L),
#'   antigen = c("CMV", "CMV")
#' )
#'
#' idata_with_antigens <- idata |>
#'   annotate_receptors(
#'     annotations = receptor_labels,
#'     annot_col = "receptor_id"
#'   )
#'
#' idata_with_antigens |>
#'   collect() |>
#'   filter(!is.na(antigen)) |>
#'   distinct(imd_receptor_id, cdr3_aa, antigen) |>
#'   arrange(imd_receptor_id)
#' # Expected result:
#' #   imd_receptor_id cdr3_aa       antigen
#' #               738 ASRAGAGTGELF  CMV
#' #              1567 ASFPVLSPYNEQF CMV
#'
#' # Match columns with different names
#' response_info <- tibble(
#'   response_code = c("FR", "PR"),
#'   response_label = c("Full response", "Partial response")
#' )
#'
#' idata_with_response <- idata |>
#'   annotate(
#'     annotations = response_info,
#'     by = c(Response = "response_code")
#'   )
#'
#' idata_with_response |>
#'   collect() |>
#'   distinct(Response, response_label) |>
#'   arrange(Response)
#' # Expected result:
#' #   Response response_label
#' #   FR       Full response
#' #   PR       Partial response
#'
#' # Replace an annotation column intentionally
#' revised_cell_labels <- tibble(
#'   barcode = c("S1_1", "S1_2"),
#'   cell_type = c("Cytotoxic T cell", "Helper T cell")
#' )
#'
#' idata_with_revised_cells <- idata_with_cells |>
#'   annotate_barcodes(
#'     annotations = revised_cell_labels,
#'     annot_col = "barcode",
#'     conflicts = "replace"
#'   )
#'
#' # Remove old repertoire summaries before defining repertoires by cell type
#' cell_repertoires <- idata |>
#'   annotate_barcodes(
#'     annotations = cell_labels,
#'     annot_col = "barcode",
#'     keep_repertoires = FALSE
#'   ) |>
#'   filter(!is.na(cell_type)) |>
#'   agg_repertoires(schema = "cell_type")
#'
#' cell_repertoires$repertoires |>
#'   arrange(cell_type)
#' # Expected result:
#' #   imd_repertoire_id cell_type  n_barcodes n_receptors
#' #                   1 CD4 T cell          1           1
#' #                   2 CD8 T cell          1           1
#'
#' @concept annotation
#' @rdname annotate_immundata
#' @export
annotate_immundata <- function(idata,
                               annotations,
                               by,
                               keep_repertoires = TRUE,
                               remove_limit = FALSE,
                               conflicts = c("error", "replace")) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_data_frame(annotations)
  checkmate::assert_character(by, min.len = 1, names = "named")
  checkmate::assert_logical(keep_repertoires)
  conflicts <- match.arg(conflicts)

  if (!remove_limit && length(annotations) >= 100) {
    rlang::abort(cli::format_inline(paste0(
      "Well, well, well, would you look at that... ",
      "Decided to dump all tens of thousands genes into your repertoire data, I guess?",
      "\nI mean, sure, do whataver you want. But I'm not responsible for the freezes or crashes.",
      "\nPass ",
      cli::col_cyan("`remove_limit = True`"),
      " to {.code left_join} to allow working with annotations of arbitrary size.",
      "\nBut remember:\n\n",
      cli::col_red("\tyou have been warned.\n\n")
    )))
  }

  ann_tbl <- as_duckdb_tibble(annotations)

  if (length(setdiff(by, colnames(ann_tbl)))) {
    cli_abort("Column(s) '{setdiff(by, colnames(ann_tbl))}' not found in annotations.")
  }
  if (!all(names(by) %in% colnames(idata$annotations))) {
    cli_abort("Column(s) '{names(by)[! names(by) %in% colnames(idata$annotations)]}' are not found in ImmunData. Please double-check the column names: {.code colnames(idata$annotations)}.")
  }

  annotation_value_cols <- setdiff(colnames(ann_tbl), unname(by))
  collisions <- intersect(annotation_value_cols, colnames(idata$annotations))
  if (length(collisions) > 0 && conflicts == "error") {
    cli_abort(
      "Annotation column(s) collide with existing ImmunData annotation columns: {.field {collisions}}. Please rename them before calling {.fn annotate_immundata}."
    )
  }
  if (length(collisions) > 0 && conflicts == "replace") {
    # `imd_group_id` is an annotation-level grouping label and is intentionally
    # replaceable. Other protected system and schema columns define receptor,
    # repertoire, or strata state and cannot be replaced safely.
    protected_collisions <- setdiff(collisions, imd_schema("group"))
    assert_mutable_annotation_columns(idata, protected_collisions)
  }

  ann_tbl <- ann_tbl |>
    rename(all_of(by))

  existing_annotations <- idata$annotations
  if (conflicts == "replace") {
    existing_annotations <- existing_annotations |>
      select(-all_of(collisions))
  }

  new_annotations <- existing_annotations |>
    left_join(ann_tbl, by = names(by))

  if (keep_repertoires) {
    clone_with_annotations(idata, new_annotations)
  } else {
    ImmunData$new(
      schema = idata$schema_receptor,
      annotations = drop_repertoire_state(new_annotations),
      provenance = get_provenance(idata)
    )
  }
}

#' @concept Annotation
#' @rdname annotate_immundata
#' @export
annotate <- annotate_immundata

#' @concept Annotation
#' @rdname annotate_immundata
#' @export
annotate_receptors <- function(idata,
                               annotations,
                               annot_col = imd_schema("receptor"),
                               keep_repertoires = TRUE,
                               remove_limit = FALSE,
                               conflicts = c("error", "replace")) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema("receptor")
  annotate(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit,
    conflicts = conflicts
  )
}

#' @concept Annotation
#' @rdname annotate_immundata
#' @export
annotate_barcodes <- function(idata,
                              annotations,
                              annot_col = "<rownames>",
                              keep_repertoires = TRUE,
                              remove_limit = FALSE,
                              conflicts = c("error", "replace")) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema("barcode")
  annotate(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit,
    conflicts = conflicts
  )
}

#' @concept Annotation
#' @rdname annotate_immundata
#' @export
annotate_chains <- function(idata,
                            annotations,
                            annot_col = imd_schema("chain"),
                            keep_repertoires = TRUE,
                            remove_limit = FALSE,
                            conflicts = c("error", "replace")) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema("chain")
  annotate(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit,
    conflicts = conflicts
  )
}
