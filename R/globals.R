#' @title Internal Immundata Global Configuration
#'
#' @description
#' `IMD_GLOBALS` is an internal list that stores globally used constants across the Immundata system.
#' It is not intended for direct use by package users, but rather to ensure consistency in schema
#' field names, default file names, and internal error messages.
#'
#' @section Components:
#' - `messages`: Named list of default messages and error texts (e.g., `"NotImpl"`).
#' - `schema`: Standardized column names for internal schema usage. These include:
#'     - `barcode`: Column name for cell barcode IDs.
#'     - `receptor`: Column name for receptor unique identifiers.
#'     - `repertoire`: Column name for repertoire group IDs.
#'     - `metadata_filename`: Column name for metadata files (internal).
#'     - `count`: Column name for receptor count per group.
#'     - `filename`: Original column name used in user metadata.
#' - `files`: Default file names used to store structured Immundata:
#'     - `receptors`: File name for receptor-level data (`receptors.parquet`).
#'     - `annotations`: File name for annotation-level data (`annotations.parquet`).
#'
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

#' @title Get Immundata Internal Schema Field Names
#'
#' @description
#' Returns the standardized field names used across Immundata objects and processing functions,
#' as defined in `IMD_GLOBALS$schema`. These include column names for barcodes, receptors,
#' repertoires, and related metadata.
#'
#' @return A named list of schema field names.
#' @export
imd_schema <- function() {
  IMD_GLOBALS$schema
}

#' @title Get Immundata Default File Names
#'
#' @description
#' Returns the standardized default filenames for storing receptor-level and annotation-level
#' data as used in `load_repertoires()` and related Immundata I/O functions.
#'
#' @return A named list of file names (e.g., `receptors.parquet`, `annotations.parquet`).
#' @export
imd_files <- function() {
  IMD_GLOBALS$files
}
