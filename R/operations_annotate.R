#' @title Join External Annotations onto an ImmunData Object
#'
#' @description
#' `annotate_immundata()` is the low‑level engine that merges an external
#' annotation table into the **annotation slot** of an [ImmunData]
#' object.  You specify one or more matching columns—typically receptor IDs
#' or cell barcodes—and the function performs a `left_join()` so that every
#' row in `idata$annotations` inherits the new information.  Convenience
#' wrappers [`annotate_receptors()`] and [`annotate_cells()`] handle the
#' common “receptor ID” and “cell barcode” use‑cases.
#'
#' @param idata            An [ImmunData] object.
#' @param annotations      A data frame or tibble with the annotations you
#'                         wish to add.
#' @param match_col        *Named* character vector where
#'                         names = column(s) in **`idata$annotations`**,
#'                         values = corresponding column(s) in
#'                         **`annotations`**.
#'                         Each pair defines a join key.
#' @param keep_repertoires Logical. After joining, should the repertoire table
#'                         be recomputed (via \code{agg_repertoires()}) so that
#'                         any new columns propagate there as well?
#' @param remove_limit     Logical.  If `FALSE` (default) the function aborts if
#'                         you try to join a *very wide* annotation table
#'                         (>\,50 columns) to guard against accidental GEX
#'                         matrices; set to `TRUE` to override.
#'
#' @return A **new** [ImmunData] object with the extra columns appended to
#'   its `annotations` table (and, if `keep_repertoires = TRUE`, to the
#'   repertoire table too).  The `receptors` table is *not* modified.
#'
#' @examples
#' \dontrun{
#' # Add clonotype assignments by receptor ID
#' ann <- data.frame(
#'   receptor_id = c(1, 5, 9),
#'   clonotype   = c("C1", "C1", "C2")
#' )
#' idata2 <- annotate_receptors(idata, ann)
#'
#' # Add a mitochondrial percentage per cell (row names as barcodes)
#' mito <- data.frame(
#'   pct_mito = runif(nrow(idata$annotations)),
#'   row.names = idata$annotations$imd_cell_id
#' )
#' idata3 <- annotate_cells(idata, mito, annot_col = "<rownames>")
#' }
#'
#' @seealso
#' * [`annotate_receptors()`] – wrapper for receptor‑level joins
#' * [`annotate_cells()`]    – wrapper for cell‑level joins
#' * [`agg_repertoires()`]   – recompute repertoire summaries
#'
#' @concept Annotation
#' @rdname annotate
#' @importFrom checkmate assert_r6 assert_data_frame assert_character assert_logical
#' @importFrom dplyr left_join rename
#' @export
annotate_immundata <- function(idata,
                               annotations,
                               match_col,
                               keep_repertoires = TRUE,
                               remove_limit = FALSE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_data_frame(annotations)
  checkmate::assert_character(match_col, min.len = 1, names = "named")
  checkmate::assert_logical(keep_repertoires)

  ann_tbl <- as_duckdb_tibble(annotations)

  if (length(setdiff(match_col, colnames(ann_tbl)))) {
    cli_abort("Column(s) '{setdiff(match_col, colnames(ann_tbl))}' not found in annotations.")
  }
  if (any(names(match_col) %in% colnames(ann_tbl))) {
    cli_abort("Column(s) '{names(match_col)[names(match_col) %in% colnames(ann_tbl))]}', reserved for joining with ImmunData, are found in the annotations. Can't rename the table, please make sure the names are unique and not already presented in annotations.")
  }
  if (!all(names(match_col) %in% colnames(idata$annotations))) {
    cli_abort("Column(s) '{names(match_col)[! names(match_col) %in% colnames(idata$annotations)]}' are not found in ImmunData. Please double-check the column names: {.code colnames(idata$annotations)}.")
  }

  ann_tbl <- ann_tbl |>
    rename(match_col)

  new_annotations <- idata$annotations %>%
    left_join(ann_tbl, by = names(match_col))

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations
  )

  if (keep_repertoires && !is.null(idata$schema_repertoire)) {
    new_idata |> agg_repertoires(idata$schema_repertoire)
  } else {
    new_idata
  }
}

#' @concept Annotation
#' @rdname annotate
#' @export
annotate_receptors <- function(idata,
                               annotations,
                               annot_col = "sequence_id",
                               keep_repertoires = TRUE,
                               remove_limit = FALSE) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema()$receptor
  annotate_immundata(
    idata = idata,
    annotations = annotations,
    match_col = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
  #   if (!remove_limit && ncol(ann_tbl) > 50) {
  #     cli_abort("Well, well, well, would you look at that... Decided to dump all tens of thousands genes into your repertoire data?
  # I mean, sure, do whataver you want. But I'm not responsible for the freezes or crashes.
  # Pass cli::col_magenta('`remove_limit = True`') to `annotate_cells` to allow working with annotations of arbitrary size.
  # But remember: cli::col_red('you have been warned').")
}

#' @concept Annotation
#' @rdname annotate
#' @export
annotate_cells <- function(idata,
                           annotations,
                           annot_col = "<rownames>",
                           keep_repertoires = TRUE,
                           remove_limit = FALSE) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema()$cell
  annotate_immundata(
    idata = idata,
    annotations = annotations,
    match_col = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
}
