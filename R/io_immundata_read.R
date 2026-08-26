#' @title Load an ImmunData object from disk
#'
#' @description
#' Continue an analysis later by reopening an [ImmunData] dataset saved on disk.
#' Use `read_immundata()` after restarting R, in another script, or when another
#' person gives you a dataset created by [write_immundata()] or
#' [read_repertoires()]. It is that simple, just don't forget to save the
#' `ImmunData` object first!
#'
#' The unit restored retains all information: chain rows,
#' cell and receptor identifiers, repertoire and stratum definitions, and
#' provenance. The function does not change these biological units or the saved
#' files. It returns a new [ImmunData] object.
#'
#' @param path A character string. Path to a saved dataset directory. The
#'   directory must contain `annotations.parquet` and `metadata.json`. When
#'   `tag` is supplied, use the project home directory that contains the
#'   `snapshots` directory. Read more about snapshots on the website.
#' @param tag A character string or `NULL`. Snapshot tag to read from
#'   `path/snapshots/<tag>/vNNN`. If `NULL`, the default, `path` itself is read.
#' @param version A non-negative integer or `NULL`. Snapshot version within
#'   `tag`. For example, `1` reads `v001`. If `NULL`, the default, the latest
#'   available version for the tag is read. `version` can only be used with
#'   `tag`.
#' @param prudence A character string. Memory protection used while reading the
#'   Parquet data. This controls whether duckplyr may convert an intermediate
#'   result from DuckDB-managed memory to an R data frame: `"stingy"`, the
#'   default here, never permits conversion; `"thrifty"` permits up to 1 million
#'   table cells (rows multiplied by columns); and `"lavish"` permits conversion
#'   regardless of size. Here, "table cells" does not mean biological cells.
#'   Passed to [duckplyr::read_parquet_duckdb()].
#' @param verbose A logical value. Whether to print progress and summary
#'   messages. Defaults to `getOption("immundata.verbose", TRUE)`.
#'
#' @details
#' Read either a dataset directory directly or a versioned snapshot within its
#' project home.
#'
#' @section Choose the saved state:
#'
#' To reopen a dataset saved directly in a folder, supply that folder as `path`
#' and leave `tag` and `version` as `NULL`.
#'
#' To reopen a managed snapshot, supply the project home as `path` and its tag.
#' By default, the latest version for that tag is read. Supply `version` when
#' you need an exact earlier state.
#'
#' @section Backend and serialized data:
#'
#' `annotations.parquet` stores the retained chain-level annotation table.
#' It is reopened as a lazy duckplyr table, so the complete table does not need
#' to be loaded into R memory. `metadata.json` stores the format and package
#' versions, receptor, repertoire, and stratum schemas, the repertoire
#' table, the snapshot identifier, lineage events, and provenance paths.
#'
#' Receptor and stratum views are reconstructed from this serialized state; they
#' are not stored as separate files. Please also mind, that the saved files
#' is an ImmunData-specific serialization, not an RDS file.
#'
#' @return A new, disk-backed [ImmunData] object representing the selected saved
#'   state. Its provenance records the directory that was read.
#'
#' @seealso [write_immundata()] for saving an analysis, [read_repertoires()] for
#'   importing AIRR-seq files, [ImmunData]
#'
#' @concept ingestion
#' @export
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Create a project home and save a filtered biological state as a snapshot
#' idata <- get_test_idata()
#' project_dir <- tempfile("immundata-project-")
#'
#' project_idata <- write_immundata(
#'   idata,
#'   output_folder = project_dir,
#'   rehome = TRUE
#' )
#'
#' fr_response <- project_idata |>
#'   filter(Response == "FR")
#'
#' write_immundata(fr_response, tag = "fr-response")
#'
#' # Read the exact first version of this snapshot
#' continued_fr <- read_immundata(
#'   project_dir,
#'   tag = "fr-response",
#'   version = 1
#' )
#'
#' continued_fr |>
#'   collect() |>
#'   summarise(
#'     n_chains = n(),
#'     n_receptors = n_distinct(imd_receptor_id)
#'   )
#' # Expected result: the snapshot contains the 955 chain rows and 871
#' # receptors from the FR response group.
#' #   n_chains n_receptors
#' #        955         871
#'
#' list.files(file.path(project_dir, "snapshots", "fr-response"))
#' # Expected result: "v001"
#'
#' unlink(project_dir, recursive = TRUE)
read_immundata <- function(path, tag = NULL, version = NULL, prudence = "stingy",
                           verbose = getOption("immundata.verbose", TRUE)) {
  checkmate::assert_character(path, len = 1, null.ok = FALSE)
  checkmate::assert_character(tag, len = 1, null.ok = TRUE)
  checkmate::assert_count(version, null.ok = TRUE)
  checkmate::assert_flag(verbose)

  resolved_path <- resolve_snapshot_input(path, tag = tag, version = version)
  if (verbose) {
    cli_alert_info("Reading ImmunData files from [{.path {resolved_path}}]")
  }

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

  if (verbose) {
    annotation_data <- read_parquet_duckdb(
      file.path(resolved_path, imd_files()$annotations),
      prudence = prudence
    )
  } else {
    annotation_data <- suppressMessages(read_parquet_duckdb(
      file.path(resolved_path, imd_files()$annotations),
      prudence = prudence
    ))
  }
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
    idata <- agg_repertoires(
      idata,
      metadata_json$schema_repertoire,
      verbose = verbose
    )
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

  idata <- set_provenance(
    idata,
    metadata_json$provenance,
    fallback_home_path = resolved_path,
    current_path = resolved_path,
    snapshot_id = metadata_json$snapshot_id,
    lineage = metadata_json$lineage
  )

  idata
}
