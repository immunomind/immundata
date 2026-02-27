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
#' @param compression Character(1) or `NULL`. Parquet compression codec passed
#'   through to DuckDB (via `duckplyr::compute_parquet(options = ...)`).
#'   Defaults to `"zstd"`. Set `NULL` to let DuckDB choose.
#' @param compression_level Numeric(1) or `NULL`. Compression level passed through
#'   to DuckDB for codecs that support levels (for example, Zstandard). Defaults
#'   to `9`. Set `NULL` to let DuckDB choose.
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
#'    `annotations.parquet` within `output_folder`.
#'    - By default, uses `compression = "zstd"` and `compression_level = 9`.
#'    - A common choice is `compression = "snappy"` for faster reads/writes
#'      with larger files.
#'    - Another common choice is `compression = "zstd"` for smaller files, often
#'      with higher CPU cost.
#'    - `compression_level` usually trades speed for size (higher levels: smaller
#'      output but slower processing).
#'    - Compatibility note: for `duckplyr` version `1.2.0`, `compute_parquet()`
#'      does not accept extra options due to a known issue. In that version,
#'      compression-related arguments are ignored and DuckDB defaults are used.
#' 6. Uses internal helper `imd_files()` to determine the standard filenames
#'    (`metadata.json`, `annotations.parquet`).
#'
#' The receptor data itself (if stored separately in future versions) is not
#' saved by this function; only the annotations linking to receptors are saved,
#' along with the schema needed to reconstruct/interpret them.
#'
#' @return
#' Invisibly returns the input `idata` object, saved to disk.
#' In other words, this allows you to create snapshots of the data in the
#' `output_folder`. Mind that by saving the object, you execute all the
#' stored computations, so this operations can take longer than expected.
#' Read more about snapshots on our website in the ["Concept" section](https://immunomind.github.io/docs/concepts/basics/immutability/).
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
#' # Optional: request a specific parquet compression setup
#' write_immundata(my_idata, save_dir, compression = "zstd", compression_level = 9)
#'
#' # Optional: let DuckDB choose both settings
#' write_immundata(my_idata, save_dir, compression = NULL, compression_level = NULL)
#'
#' # Check the created files
#' list.files(save_dir) # Should show "annotations.parquet" and "metadata.json"
#'
#' # Clean up
#' unlink(save_dir, recursive = TRUE)
#' }
write_immundata <- function(idata, output_folder, compression = "zstd", compression_level = 9) {
  compression_was_provided <- !missing(compression)
  compression_level_was_provided <- !missing(compression_level)

  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(output_folder,
    max.len = 1,
    null.ok = FALSE
  )
  checkmate::assert_character(compression,
    max.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_numeric(compression_level,
    len = 1,
    null.ok = TRUE
  )

  output_folder <- normalizePath(output_folder, mustWork = FALSE)
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  metadata_path <- file.path(output_folder, imd_files()$metadata)
  annotations_path <- file.path(output_folder, imd_files()$annotations)

  metadata_json <- list(
    version = jsonlite::unbox(as.character(packageVersion("immundata"))),
    receptor_schema = idata$schema_receptor,
    repertoire_schema = idata$schema_repertoire
  )

  cli::cli_alert_info("Writing the receptor annotation data to [{annotations_path}]")
  duckplyr_is_1_2_0 <- isTRUE(utils::packageVersion("duckplyr") == "1.2.0")
  parquet_options <- Filter(
    f = function(x) !is.null(x),
    x = list(
      compression = compression,
      compression_level = compression_level
    )
  )

  if (duckplyr_is_1_2_0) {
    if (compression_was_provided || compression_level_was_provided) {
      cli::cli_alert_warning(
        "duckplyr 1.2.0 does not accept compression options in `compute_parquet()`; ignoring `compression` and `compression_level`."
      )
    }
    compute_parquet(
      idata$annotations,
      annotations_path
    )
  } else if (length(parquet_options) == 0) {
    compute_parquet(
      idata$annotations,
      annotations_path
    )
  } else {
    compute_parquet(
      idata$annotations,
      annotations_path,
      options = parquet_options
    )
  }

  cli::cli_alert_info("Writing the metadata to [{metadata_path}]")
  jsonlite::write_json(metadata_json, metadata_path, null = "null")

  cli::cli_alert_success("ImmunData files saved to [{output_folder}]")

  invisible(read_immundata(output_folder))
}
