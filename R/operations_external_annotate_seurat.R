#' @title Annotate a Seurat object from ImmunData (by barcode)
#'
#' @description
#' Copy selected columns from `idata$annotations` to Seurat metadata using
#' the cell barcode. This is the simplest way to transfer data from `immundata`
#' to Seurat object, e.g., for plotting data on UMAP.
#'
#' @param idata An [immundata::ImmunData] object.
#' @param sdata A Seurat object (cells are columns; barcodes are `colnames(sdata)`).
#' @param cols Character vector with column names to transfer from `idata$annotations`.
#'   Typical choices: `"clonal_prop_bin"` or `"clonal_rank_bin"`.
#'
#' @return The updated Seurat object with new metadata columns.
#'
#' @seealso
#' [immunarch::annotate_clonality_prop], [immunarch::annotate_clonality_prop],
#' [ImmunData], [SeuratObject::AddMetaData]
#'
#' @examples
#' \dontrun{
#' # After annotating receptors:
#' idata <- annotate_clonality_prop(idata)
#'
#' # Transfer the clonality bin to Seurat and plot:
#' sdata <- annotate_seurat(idata, sdata, cols = "clonal_prop_bin")
#' Seurat::DimPlot(sdata, reduction = "umap", group.by = "clonal_prop_bin", shuffle = TRUE)
#'
#' # Alternative: rank bins
#' idata <- annotate_clonality_rank(idata, bins = c(10, 100))
#' sdata <- annotate_seurat(idata, sdata, cols = "clonal_rank_bin")
#' }
#'
#' @concept Annotation
#' @export
annotate_seurat <- function(idata,
                            sdata,
                            cols) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_class(sdata, classes = "Seurat")
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE)

  ann <- idata$annotations
  bcsym <- immundata::imd_schema_sym("barcode")
  bcname <- immundata::imd_schema("barcode")

  missing_cols <- setdiff(c(bcname, cols), colnames(ann))
  if (length(missing_cols) > 0) {
    rlang::abort(
      cli::format_inline("Column(s) {cli::col_cyan(missing_cols)} not found in idata$annotations.")
    )
  }

  # TODO: mind that we use distinct() here - there could be edge cases
  df <- ann |>
    dplyr::select(barcode = !!bcsym, dplyr::all_of(cols)) |>
    dplyr::distinct(.data$barcode, .keep_all = TRUE) |>
    collect()

  meta <- as.data.frame(df[, cols, drop = FALSE])
  rownames(meta) <- df$barcode

  SeuratObject::AddMetaData(sdata, metadata = meta)
}
