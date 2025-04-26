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
#' @exportS3Method dplyr::left_join
left_join.ImmunData <- function(idata,
                                annotations,
                                by,
                                keep_repertoires = TRUE,
                                remove_limit = FALSE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_data_frame(annotations)
  checkmate::assert_character(by, min.len = 1, names = "named")
  checkmate::assert_logical(keep_repertoires)

  if (!remove_limit && ncol(annotations) >= 100) {
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
  if (any(names(by) %in% colnames(ann_tbl))) {
    same_col_names <- intersect(names(by), colnames(ann_tbl))
    if (!all(by[names(by)] == names(by))) {
      # We don't care about the very same column names, we are try to mitigate risk when there is a collision after (!) the renaming
      cli_abort("Column(s) '{names(by)[names(by) %in% colnames(ann_tbl)]}', reserved for joining with ImmunData, are found in the annotations. Can't rename the table, please make sure the names are unique and not already presented in annotations.")
    }
  }
  if (!all(names(by) %in% colnames(idata$annotations))) {
    cli_abort("Column(s) '{names(by)[! names(by) %in% colnames(idata$annotations)]}' are not found in ImmunData. Please double-check the column names: {.code colnames(idata$annotations)}.")
  }

  ann_tbl <- ann_tbl |>
    rename(all_of(by))

  new_annotations <- idata$annotations %>%
    left_join(ann_tbl, by = names(by))

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
#' @rdname left_join.ImmunData
#' @export
annotate_receptors <- function(idata,
                               annotations,
                               annot_col = imd_schema("receptor"),
                               keep_repertoires = TRUE,
                               remove_limit = FALSE) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema("receptor")
  left_join(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
}

#' @concept Annotation
#' @rdname left_join.ImmunData
#' @export
annotate_barcodes <- function(idata,
                              annotations,
                              annot_col = "<rownames>",
                              keep_repertoires = TRUE,
                              remove_limit = FALSE) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema("barcode")
  left_join(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
}

#' @concept Annotation
#' @rdname left_join.ImmunData
#' @export
annotate_chains <- function(idata,
                            annotations,
                            annot_col = imd_schema("chain"),
                            keep_repertoires = TRUE,
                            remove_limit = FALSE) {
  if (annot_col == "<rownames>") {
    annotations[["imd_row_names"]] <- rownames(annotations)
    annot_col <- "imd_row_names"
  }
  match_col <- c(annot_col)
  names(match_col) <- imd_schema("chain")
  left_join(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
}
