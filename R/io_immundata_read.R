#' @title Load a saved ImmunData from disk
#'
#' @description
#' Reconstructs an `ImmunData` object from files previously saved to a directory
#' by [write_immundata()] or the internal saving step of [read_repertoires()].
#' It reads the `annotations.parquet` file for the main data and `metadata.json`
#' to retrieve the necessary receptor and repertoire schemas.
#'
#' @param path Character(1). Path to the **directory** containing the saved
#'   `ImmunData` files (`annotations.parquet` and `metadata.json`).
#' @param prudence Character(1). Controls strictness of type inference when
#'   reading the Parquet file, passed to `duckplyr::read_parquet_duckdb()`.
#'   Default `"stingy"` likely implies stricter type checking or safer inference.
#' @param verbose Logical(1). If `TRUE` (default), prints informative messages
#'   using `cli` during loading. Set to `FALSE` for quiet operation.
#'
#' @details
#' This function expects a directory structure created by [write_immundata()],
#' containing at least:
#' - `annotations.parquet`: The main annotation data table.
#' - `metadata.json`: Contains package version, receptor schema, and optionally
#'   repertoire schema.
#'
#' The loading process involves:
#' 1. Checking that the specified `path` is a directory and contains the
#'    required `annotations.parquet` and `metadata.json` files.
#' 2. Reading `metadata.json` using `jsonlite::read_json()`.
#' 3. Reading `annotations.parquet` using `duckplyr::read_parquet_duckdb()` with
#'    the specified `prudence` level.
#' 4. Extracting the `receptor_schema` and `repertoire_schema` from the loaded
#'    metadata.
#' 5. Instantiating a new `ImmunData` object using the loaded `annotations` data
#'    and the `receptor_schema`.
#' 6. If a non-empty `repertoire_schema` was found in the metadata, it calls
#'    [agg_repertoires()] on the newly created object to recalculate and
#'    attach repertoire-level information based on that schema.
#'
#' @return A new `ImmunData` object reconstructed from the saved files. If
#'   repertoire information was saved, it will be recalculated and included.
#'
#' @seealso [write_immundata()] for saving `ImmunData` objects,
#'   [read_repertoires()] for the primary data loading pipeline, [ImmunData] class,
#'   [agg_repertoires()] for repertoire definition.
#'
#' @concept ingestion
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume 'my_idata' is an ImmunData object created previously
#' # my_idata <- read_repertoires(...)
#'
#' # Define a temporary directory for saving
#' save_dir <- tempfile("saved_immundata_")
#'
#' # Save the ImmunData object
#' write_immundata(my_idata, save_dir)
#'
#' # --- Later, in a new session or script ---
#'
#' # Load the ImmunData object back from the directory
#' loaded_idata <- read_immundata(save_dir)
#'
#' # Verify the loaded object
#' print(loaded_idata)
#' # compare_methods(my_idata$annotations, loaded_idata$annotations) # If available
#'
#' # Clean up
#' unlink(save_dir, recursive = TRUE)
#' }
read_immundata <- function(path, prudence = "stingy", verbose = TRUE) {
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

  if (verbose) {
    cli_alert_success("Loaded ImmunData with the receptor schema: [{receptor_schema}]")
  }

  if (length(repertoire_schema) > 0) {
    idata <- agg_repertoires(idata, repertoire_schema)

    if (verbose) {
      cli_alert_success("Loaded ImmunData with the repertoire schema: [{repertoire_schema}]")
    }
  }

  idata
}
