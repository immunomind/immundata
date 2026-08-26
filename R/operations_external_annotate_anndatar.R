#' @title Annotate an AnnData object from ImmunData (by barcode)
#'
#' @description
#' Copy selected columns from `idata$annotations` to `adata$obs`, matching by
#' cell barcode (`adata$obs_names`).
#'
#' @param idata An [immundata::ImmunData] object.
#' @param adata An [anndataR::AbstractAnnData] object.
#' @param cols Character vector with column names to transfer from
#'   `idata$annotations`.
#'
#' @return The updated AnnData object.
#'
#' @export
annotate_anndata <- function(idata,
                             adata,
                             cols) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_true(inherits(adata, "AbstractAnnData"))
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE)

  obs_names <- adata$obs_names
  if (is.null(obs_names) || length(obs_names) == 0) {
    cli::cli_abort("`adata$obs_names` is missing or empty. Expected cell barcodes in obs_names.")
  }
  if (any(is.na(obs_names)) || any(obs_names == "")) {
    cli::cli_abort("`adata$obs_names` contains NA/empty values. Expected valid barcodes.")
  }
  if (anyDuplicated(obs_names) > 0) {
    cli::cli_abort("`adata$obs_names` must be unique (duplicate barcodes found).")
  }

  ann <- idata$annotations
  bcsym <- immundata::imd_schema_sym("barcode")
  bcname <- immundata::imd_schema("barcode")

  missing_cols <- setdiff(c(bcname, cols), colnames(ann))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column(s) {cli::col_cyan(missing_cols)} not found in idata$annotations."
    )
  }

  # TODO: I use distinct() here, any edge cases?
  # Keep first record per barcode (same idea as annotate_seurat)
  df <- ann |>
    dplyr::select(barcode = !!bcsym, dplyr::all_of(cols)) |>
    dplyr::distinct(.data$barcode, .keep_all = TRUE) |>
    collect()

  obs <- adata$obs
  idx <- match(obs_names, df$barcode)

  for (nm in cols) {
    obs[[nm]] <- df[[nm]][idx]
  }

  adata$obs <- obs
  adata
}
