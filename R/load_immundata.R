#' @title Load a Saved ImmunData Object from Disk
#'
#' @description
#' Loads an `ImmunData` object from a directory created by Immundata I/O functions
#' (e.g., `load_repertoires()` with file splitting or saving). It reads receptor-level
#' and annotation-level data from standardized Parquet files and reconstructs a
#' new `ImmunData` object with inferred schema.
#'
#' This function expects two files inside the provided directory:
#' - `receptors.parquet`: contains the receptor-level table
#' - `annotations.parquet`: contains the annotation-level table
#'
#' These filenames are defined in `imd_files()` and follow the Immundata storage convention.
#'
#' @param path Path to the folder containing the saved ImmunData files.
#' @param verbose Logical. If `TRUE` (default), prints progress messages to the console.
#'
#' @return A new `ImmunData` object containing receptor and annotation data.
#'
#' @seealso [imd_files()], [ImmunData], [read_parquet_duckdb()]
#'
#' @export
load_immundata <- function(path, verbose = TRUE) {
  if (verbose) {
    cli_alert_info("Loading ImmunData files from [{.path {path}}]")
  }

  assert_directory_exists(path)

  assert_file_exists(file.path(path, imd_files()$receptors))
  assert_file_exists(file.path(path, imd_files()$annotations))

  receptor_data <- read_parquet_duckdb(file.path(path, imd_files()$receptors), prudence = "stingy")
  annotation_data <- read_parquet_duckdb(file.path(path, imd_files()$annotations), prudence = "stingy")

  schema <- receptor_data |> colnames()

  if (verbose) {
    cli_alert_success("Loaded ImmunData with the schema: [{schema}]")
  }

  ImmunData$new(
    receptors = receptor_data,
    annotations = annotation_data,
    schema = schema
  )
}
