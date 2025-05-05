#' @title Annotate ImmunData object
#'
#' @description Joins additional annotation data to the annotations slot of an `ImmunData` object.
#'
#' This function allows you to add extra information to your repertoire data by joining a
#' dataframe of annotations based on specified columns. It supports joining by
#' one or more columns.
#'
#' @param idata An `ImmunData` R6 object containing repertoire and annotation data.
#' @param annotations A data frame containing the annotations to be joined.
#' @param by A named character vector specifying the columns to join by. The names of the
#'   vector should be the column names in `idata$annotations` and the values should be
#'   the corresponding column names in the `annotations` data frame.
#' @param annot_col A character vector specifying the column with receptor, barcode or chain identifiers
#'   to annotate a corresponding receptors, barode or chains in `idata`.
#' @param keep_repertoires Logical. If `TRUE` (default) and the `ImmunData` object
#'   contains repertoire data (`idata$schema_repertoire` is not NULL), the repertoires
#'   will be re-aggregated after joining the annotations. Set to `FALSE` if you do not
#'   want to re-aggregate repertoires immediately.
#' @param remove_limit Logical. If `FALSE` (default), a warning will be issued if the
#'   `annotations` data frame has 100 or more columns, suggesting potential performance
#'   issues. Set to `TRUE` to disable this warning and allow joining of annotations
#'   with an arbitrary number of columns. Use with caution, as joining wide dataframes
#'   can be memory-intensive and slow.
#'
#' @return A new `ImmunData` object with the annotations joined to the `annotations` slot.
#'
#' @details The function performs a left join operation, keeping all rows from
#'   `idata$annotations` and adding matching columns from the `annotations` data frame.
#'   If there are multiple matches in `annotations` for a row in `idata$annotations`,
#'   all combinations will be returned, potentially increasing the number of rows
#'   in the resulting annotations table.
#'
#'   The function uses `checkmate` to validate the input types and structure.
#'
#'   A check is performed to ensure that the columns specified in `by` exist in both
#'   `idata$annotations` and the `annotations` data frame.
#'
#'   The `annotations` data frame is converted to a duckdb tibble internally for
#'   efficient joining, especially with large datasets.
#'
#' @section Warning:
#' By default (`remove_limit = FALSE`), joining an `annotations` data frame with 100 or
#' more columns will trigger a warning. This is a safeguard to prevent accidental
#' joining of very wide data (e.g., gene expression data) that could lead to
#' performance degradation or crashes. If you understand the risks and intend to join
#' a wide data frame, set `remove_limit = TRUE`.
#'
#' @concept Annotation
#' @examples
#' \dontrun{
#' # Assuming 'my_immun_data' is an ImmunData object and 'sample_info' is a data frame
#' # with a column 'sample_id' matching 'sample' in my_immun_data$annotations
#' # and additional columns like 'treatment' and 'disease_status'.
#'
#' sample_info <- data.frame(
#'   sample_id = c("sample1", "sample2", "sample3", "sample4"),
#'   treatment = c("Treatment A", "Treatment B", "Treatment A", "Treatment C"),
#'   disease_status = c("Healthy", "Disease", "Healthy", "Disease"),
#'   stringsAsFactors = FALSE # Important to keep characters as characters
#' )
#'
#' # Join sample information using the 'sample' column
#' my_immun_data_annotated <- annotate(
#'   idata = my_immun_data,
#'   annotations = sample_info,
#'   by = c("sample" = "sample_id")
#' )
#'
#' # New sample_info
#'
#' # Join data by multiple columns, e.g., 'sample' and 'barcode'
#' # Assuming 'cell_annotations' is a data frame with 'sample_barcode' and 'cell_type'
#' my_immun_data_cell_annotated <- annotate(
#'   idata = my_immun_data,
#'   annotations = cell_annotations,
#'   by = c("sample" = "sample", "barcode" = "sample_barcode")
#' )
#'
#' # Join a wide dataframe, suppressing the column limit warning
#' # Assuming 'gene_expression' is a data frame with 'barcode' and many gene columns
#' my_immun_data_gene_expression <- annotate(
#'   idata = my_immun_data,
#'   annotations = gene_expression,
#'   by = c("barcode" = "barcode"),
#'   remove_limit = TRUE
#' )
#' }
#'
#' @export
annotate_immundata <- function(idata,
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
#' @rdname annotate_immundata
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
  annotate(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
}

#' @concept Annotation
#' @rdname annotate_immundata
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
  annotate(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
}

#' @concept Annotation
#' @rdname annotate_immundata
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
  annotate(
    idata = idata,
    annotations = annotations,
    by = match_col,
    keep_repertoires = keep_repertoires,
    remove_limit = remove_limit
  )
}
