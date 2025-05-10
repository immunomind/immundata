#' @title Save ImmunData to disk
#'
#' @description
#' Serializes the essential components of an `ImmunData` object to disk for
#' efficient storage and later retrieval. It saves the core annotation data
#' (`idata$annotations`) as a compressed Parquet file and accompanying metadata
#' (including receptor/repertoire schemas and package version) as a JSON file
#' within a specified directory.
#'
#' @param idata The `ImmunData` object to save. Must be an R6 object of class
#'   `ImmunData` containing at least the `$annotations` table and schema information
#'   (`$schema_receptor`, optionally `$schema_repertoire`).
#' @param output_folder Character(1). Path to the directory where the output files
#'   will be written. If the directory does not exist, it will be created
#'   recursively.
#'
#' @details
#' The function performs the following actions:
#' 1. Validates the input `idata` object and `output_folder` path.
#' 2. Creates the `output_folder` if it doesn't exist.
#' 3. Constructs a list containing metadata: `immundata` package version,
#'    receptor schema (`idata$schema_receptor`), and repertoire schema
#'    (`idata$schema_repertoire`).
#' 4. Writes the metadata list to `metadata.json` within `output_folder`.
#' 5. Writes the `idata$annotations` table (a `duckplyr_df` or similar) to
#'    `annotations.parquet` within `output_folder`. Uses Zstandard compression
#'    (`compression = "zstd"`, `compression_level = 9`) for a good balance
#'    between file size and read/write speed.
#' 6. Uses internal helper `imd_files()` to determine the standard filenames
#'    (`metadata.json`, `annotations.parquet`).
#'
#' The receptor data itself (if stored separately in future versions) is not
#' saved by this function; only the annotations linking to receptors are saved,
#' along with the schema needed to reconstruct/interpret them.
#'
#' @return
#' Invisibly returns the input `idata` object. Its primary effect is creating
#' `metadata.json` and `annotations.parquet` files in the `output_folder`.
#'
#' @seealso [read_immundata()] for loading the saved data, [read_repertoires()]
#'   which uses this function internally, [ImmunData] class definition.
#'
#' @concept ingestion
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume 'my_idata' is an ImmunData object created previously
#' # my_idata <- read_repertoires(...)
#'
#' # Define an output directory
#' save_dir <- tempfile("saved_immundata_")
#'
#' # Save the ImmunData object
#' write_immundata(my_idata, save_dir)
#'
#' # Check the created files
#' list.files(save_dir) # Should show "annotations.parquet" and "metadata.json"
#'
#' # Clean up
#' unlink(save_dir, recursive = TRUE)
#' }
write_immundata <- function(idata, output_folder) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(output_folder,
    max.len = 1,
    null.ok = FALSE
  )

  output_folder <- normalizePath(output_folder, mustWork = FALSE)
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  metadata_path <- file.path(output_folder, imd_files()$metadata)
  annotations_path <- file.path(output_folder, imd_files()$annotations)

  metadata_json <- list(
    version = as.character(packageVersion("immundata")),
    receptor_schema = idata$schema_receptor,
    repertoire_schema = idata$schema_repertoire
  )

  cli::cli_alert_info("Writing the receptor annotation data to [{annotations_path}]")
  compute_parquet(idata$annotations,
    annotations_path,
    options = list(
      compression = "zstd",
      compression_level = 9
    )
  )

  cli::cli_alert_info("Writing the metadata to [{metadata_path}]")
  jsonlite::write_json(metadata_json, metadata_path)

  cli::cli_alert_success("ImmunData files saved to [{output_folder}]")

  invisible(idata)
}
