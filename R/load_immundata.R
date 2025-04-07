#' @export
load_immundata <- function(foldername, schema, verbose = TRUE) {
  if (verbose) {
    cli_alert_info("Loading ImmunData files from [{.path {foldername}}]")
  }

  assert_directory_exists(foldername)

  assert_file_exists(file.path(foldername, imd_files()$receptors))
  assert_file_exists(file.path(foldername, imd_files()$annotations))

  receptor_data <- read_parquet_duckdb(file.path(foldername, imd_files()$receptors))
  annotation_data <- read_parquet_duckdb(file.path(foldername, imd_files()$annotations))

  ImmunData$new(
    receptors = receptor_data,
    annotations = annotation_data,
    schema = schema
  )
}
