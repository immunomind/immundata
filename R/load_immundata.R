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
