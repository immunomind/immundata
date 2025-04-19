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
#'
#' @return A new `ImmunData` object containing receptor and annotation data.
#'
#' @seealso [read_repertoires()], [imd_files()], [ImmunData], [duckplyr::read_parquet_duckdb()]
#'
#' @export
read_immundata <- function(path, prudence = "stingy") {
  cli_alert_info("Reading ImmunData files from [{.path {path}}]")

  assert_directory_exists(path)
  assert_file_exists(file.path(path, imd_files()$annotations))
  assert_file_exists(file.path(path, imd_files()$metadata))

  metadata_json <- jsonlite::read_json(file.path(path, imd_files()$metadata), simplifyVector = T)
  annotation_data <- read_parquet_duckdb(file.path(path, imd_files()$annotations), prudence = prudence)

  receptor_schema <- metadata_json[[imd_meta_schema()$receptor_schema]]
  # TODO: run checks/repairs: 1) no receptor schema, need to aggregate; 2) wrong columns; 3) receptor schema but no imd_receptor_id

  repertoire_schema <- metadata_json[[imd_meta_schema()$repertoire_schema]]

  idata <- ImmunData$new(
    schema = receptor_schema,
    annotations = annotation_data
  )

  cli_alert_success("Loaded ImmunData with the receptor schema: [{receptor_schema}]")

  if (length(repertoire_schema) > 0) {
    idata <- agg_repertoires(idata, repertoire_schema)
    cli_alert_success("Loaded ImmunData with the repertoire schema: [{repertoire_schema}]")
  }

  idata
}
