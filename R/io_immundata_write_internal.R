#' @title Internal writer for ImmunData snapshots
#' @description Internal helper used by `write_immundata()` and `read_repertoires()`
#' to write `metadata.json` (including repertoires) and `annotations.parquet`.
#' @keywords internal
#' @noRd
write_immundata_internal <- function(idata,
                                     output_folder = NULL,
                                     snapshot_tag = NULL,
                                     rehome = FALSE,
                                     compression = "zstd",
                                     compression_level = 9,
                                     producer_function = "write_immundata",
                                     ingestion_payload = NULL,
                                     metadata_extensions = NULL) {
  compression_was_provided <- !missing(compression)
  compression_level_was_provided <- !missing(compression_level)

  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(output_folder,
    max.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_character(snapshot_tag,
    max.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_flag(rehome)
  checkmate::assert_character(producer_function,
    len = 1,
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
  checkmate::assert_list(ingestion_payload, null.ok = TRUE)
  checkmate::assert_list(metadata_extensions, null.ok = TRUE)

  resolved_output <- resolve_snapshot_output_folder(
    idata = idata,
    output_folder = output_folder,
    tag = snapshot_tag,
    rehome = rehome
  )
  output_folder <- resolved_output$output_folder
  snapshot_tag <- resolved_output$tag
  provenance_before <- resolved_output$provenance
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  metadata_path <- file.path(output_folder, imd_files()$metadata)
  annotations_path <- file.path(output_folder, imd_files()$annotations)

  snapshot_metadata <- build_snapshot_metadata(
    idata = idata,
    producer_function = producer_function,
    provenance_before = provenance_before,
    output_folder = output_folder,
    snapshot_tag = snapshot_tag,
    rehome = rehome,
    ingestion_payload = ingestion_payload,
    metadata_extensions = metadata_extensions
  )
  metadata_json <- snapshot_metadata$metadata
  provenance_after <- snapshot_metadata$provenance

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
  jsonlite::write_json(metadata_json, metadata_path, null = "null", auto_unbox = TRUE, pretty = TRUE)

  cli::cli_alert_success("ImmunData files saved to [{output_folder}]")

  written_idata <- read_immundata(output_folder, verbose = FALSE)
  invisible(set_provenance(written_idata, provenance_after))
}
