#' @title Save ImmunData to disk
#'
#' @description
#' Serializes the essential components of an `ImmunData` object to disk for
#' efficient storage and later retrieval. It saves the core annotation data
#' (`idata$annotations`) as a compressed Parquet file and accompanying metadata
#' (including schemas, repertoire data, and package version) as a JSON file
#' within a specified directory.
#'
#' @param idata The `ImmunData` object to save. Must be an R6 object of class
#'   `ImmunData` containing at least the `$annotations` table and schema information
#'   (`$schema_receptor`, optionally `$schema_repertoire` and `$schema_strata`).
#' @param output_folder Character(1) or `NULL`. Path to the directory where the
#'   output files will be written. If `NULL`, a snapshot directory is created as
#'   `home_path/snapshots/<tag>/vNNN`, where `home_path` is read from internal
#'   `ImmunData` provenance.
#' @param tag Character(1) or `NULL`. Snapshot tag used only when
#'   `output_folder = NULL` (for example, `"baseline"`). If `NULL`, defaults to
#'   `"default"` for auto-snapshots.
#' @param rehome Logical(1). If `TRUE`, and `output_folder` is explicitly
#'   provided, this folder becomes the new snapshot home for future auto-snapshots.
#'   Default: `FALSE`.
#' @param compression Character(1) or `NULL`. Parquet compression codec passed
#'   through to DuckDB (via `duckplyr::compute_parquet(options = ...)`).
#'   Defaults to `"zstd"`. Set `NULL` to let DuckDB choose.
#' @param compression_level Numeric(1) or `NULL`. Compression level passed through
#'   to DuckDB for codecs that support levels (for example, Zstandard). Defaults
#'   to `9`. Set `NULL` to let DuckDB choose.
#' @param verbose Logical(1). Whether to print informative messages. Defaults to
#'   `getOption("immundata.verbose", TRUE)`.
#'
#' @details
#' The function performs the following actions:
#' 1. Validates the input `idata` object and write options.
#' 2. Resolves the destination folder:
#'    - uses `output_folder` when explicitly provided, or
#'    - creates an auto-snapshot folder under
#'      `home_path/snapshots/<tag>/vNNN` when `output_folder = NULL`.
#' 3. Constructs metadata including schemas, the repertoire table, `snapshot_id`,
#'    lineage, and provenance paths.
#' 4. Writes metadata to `metadata.json` within the resolved output folder.
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
#' The receptor data itself is not stored separately; receptors remain a view
#' derived from annotations and the receptor schema.
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
#' # Auto-snapshot under <home>/snapshots/baseline/vNNN
#' write_immundata(my_idata, tag = "baseline")
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
write_immundata <- function(idata,
                            output_folder = NULL,
                            tag = NULL,
                            rehome = FALSE,
                            compression = "zstd",
                            compression_level = 9,
                            verbose = getOption("immundata.verbose", TRUE)) {
  write_immundata_internal(
    idata = idata,
    output_folder = output_folder,
    snapshot_tag = tag,
    rehome = rehome,
    compression = compression,
    compression_level = compression_level,
    producer_function = "write_immundata",
    verbose = verbose
  )
}
