#' @keywords internal
IMD_GLOBALS <- list(
  messages = list(
    "NotImpl" = "Error: the method is not implemented."
  ),
  schema = list(
    barcode = "imd_barcode",
    receptor = "imd_receptor_id",
    repertoire = "imd_repertoire_id",
    metadata_filename = "imd_filename",
    count = "imd_count",
    filename = "filename"
  ),
  files = list(
    receptors = "receptors.parquet",
    annotations = "annotations.parquet"
  )
)

#' @export
imd_schema <- function() {
  IMD_GLOBALS$schema
}

#' @export
imd_files <- function() {
  IMD_GLOBALS$files
}
