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
#' @param tag Character(1) or `NULL`. Optional snapshot tag to load from
#'   `path/snapshots/<tag>/vNNN`. When provided, `path` must point to the
#'   project/home folder.
#' @param version Integer(1) or `NULL`. Optional snapshot version number to
#'   load within a tag (e.g. `1` means `v001`). If `NULL`, the latest version
#'   for the tag is loaded.
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
#' - `metadata.json`: Contains package version, receptor/repertoire/strata
#'   schemas, the repertoire table, current `snapshot_id`, lineage events, and
#'   provenance paths.
#'
#' The loading process involves:
#' 1. Checking that the specified `path` is a directory and contains the
#'    required `annotations.parquet` and `metadata.json` files.
#' 2. Reading `metadata.json` using `jsonlite::read_json()`.
#' 3. Reading `annotations.parquet` using `duckplyr::read_parquet_duckdb()` with
#'    the specified `prudence` level.
#' 4. Restoring the receptor, repertoire, and strata schemas and the serialized
#'    repertoire table from metadata.
#' 5. Instantiating a new `ImmunData` object directly, without re-aggregating
#'    repertoires or strata.
#'
#' @return A new `ImmunData` object reconstructed from the saved files.
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
read_immundata <- function(path, tag = NULL, version = NULL, prudence = "stingy", verbose = TRUE) {
  checkmate::assert_character(path, len = 1, null.ok = FALSE)
  checkmate::assert_character(tag, len = 1, null.ok = TRUE)
  checkmate::assert_count(version, null.ok = TRUE)

  resolved_path <- imd_resolve_snapshot_input(path, tag = tag, version = version)
  cli_alert_info("Reading ImmunData files from [{.path {resolved_path}}]")

  assert_directory_exists(resolved_path)
  assert_file_exists(file.path(resolved_path, imd_files()$annotations))
  assert_file_exists(file.path(resolved_path, imd_files()$metadata))

  metadata_path <- file.path(resolved_path, imd_files()$metadata)
  meta_raw <- jsonlite::read_json(
    metadata_path,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  metadata_json <- normalize_metadata_json(meta_raw)

  annotation_data <- read_parquet_duckdb(file.path(resolved_path, imd_files()$annotations), prudence = prudence)
  validate_snapshot_columns(metadata_json, annotation_data, resolved_path)

  receptor_schema <- metadata_json[["schema_receptor"]]

  strata_schema <- metadata_json[["schema_strata"]]
  repertoire_data <- metadata_json[["repertoires"]]
  if (!is.null(repertoire_data)) {
    repertoire_data <- duckplyr::as_duckdb_tibble(repertoire_data)
  }

  strata_data <- NULL
  if (!is.null(strata_schema)) {
    strata_data <- repertoire_data |>
      select(all_of(c(
        imd_schema("strata"),
        imd_schema("strata_name"),
        strata_schema
      ))) |>
      distinct()
  }

  idata <- ImmunData$new(
    schema = receptor_schema,
    annotations = annotation_data,
    repertoires = repertoire_data,
    strata = strata_data
  )

  if (isTRUE(metadata_json$rebuild_repertoires)) {
    idata <- agg_repertoires(idata, metadata_json$schema_repertoire)
  }

  if (verbose) {
    cli_alert_success("Loaded ImmunData with the receptor schema: [{receptor_schema}]")
  }

  if (!is.null(idata$schema_repertoire) && length(idata$schema_repertoire) > 0) {
    if (verbose) {
      cli_alert_success("Loaded ImmunData with the repertoire schema: [{idata$schema_repertoire}]")
    }
  }

  if (!is.null(idata$schema_strata) && length(idata$schema_strata) > 0 && verbose) {
    cli_alert_success("Loaded ImmunData with the strata schema: [{idata$schema_strata}]")
  }

  provenance <- normalize_provenance(
    metadata_json$provenance,
    fallback_home_path = if (is.null(metadata_json$provenance$home_path)) resolved_path else metadata_json$provenance$home_path,
    fallback_current_path = resolved_path,
    snapshot_id = metadata_json$snapshot_id,
    lineage = metadata_json$lineage
  )
  provenance$current_path <- normalizePath(resolved_path, mustWork = FALSE)
  if (is.null(provenance$home_path)) {
    provenance$home_path <- provenance$current_path
  }
  provenance$snapshot_root <- normalizePath(file.path(provenance$home_path, "snapshots"), mustWork = FALSE)

  idata <- imd_set_provenance(idata, provenance)

  idata
}
