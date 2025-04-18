#' @title Load a Saved ImmunData Object from Disk
#'
#' @description
#' Loads an `ImmunData` object from a directory created by Immundata I/O functions
#' (e.g., `read_repertoires()` with file splitting or saving). It reads receptor-level
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
#' @param repertoire_schema Character vector. Vector of column names to aggregate repertoires by.
#'
#' @return A new `ImmunData` object containing receptor and annotation data.
#'
#' @seealso [read_repertoires()], [imd_files()], [ImmunData], [duckplyr::read_parquet_duckdb()]
#'
#' @export
read_immundata <- function(path, repertoire_schema = NULL) {
  cli_alert_info("Reading ImmunData files from [{.path {path}}]")

  assert_directory_exists(path)

  assert_file_exists(file.path(path, imd_files()$receptors))
  assert_file_exists(file.path(path, imd_files()$annotations))

  assert(
    test_character(repertoire_schema,
                   null.ok = TRUE
    ),
    test_function(repertoire_schema)
  )

  receptor_data <- read_parquet_duckdb(file.path(path, imd_files()$receptors), prudence = "stingy")
  annotation_data <- read_parquet_duckdb(file.path(path, imd_files()$annotations), prudence = "stingy")

  schema <- receptor_data |> colnames()
  schema <- setdiff(schema, imd_schema()$receptor)

  cli_alert_success("Loaded ImmunData with the receptor schema: [{schema}]")

  idata <- ImmunData$new(
    receptors = receptor_data,
    annotations = annotation_data,
    schema = schema
  )

  if (!is.null(repertoire_schema)) {
    idata <- agg_repertoires(idata, repertoire_schema)
    cli_alert_success("Loaded ImmunData with the repertoire schema: [{repertoire_schema}]")
  }

  idata
}
