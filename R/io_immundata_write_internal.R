#' @title Internal writer for ImmunData snapshots
#' @description Internal helper used by `write_immundata()` and `read_repertoires()`
#' to write `metadata.json` and `annotations.parquet`.
#' @keywords internal
#' @noRd
write_immundata_internal <- function(idata,
                                     output_folder,
                                     compression = "zstd",
                                     compression_level = 9,
                                     producer_function = "write_immundata",
                                     metadata_lineage_inputs = NULL,
                                     metadata_lineage_args = NULL,
                                     metadata_lineage_columns = NULL,
                                     metadata_lineage_pipeline = NULL,
                                     metadata_extensions = NULL) {
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
  checkmate::assert_character(producer_function,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_list(metadata_lineage_inputs, null.ok = TRUE)
  checkmate::assert_list(metadata_lineage_args, null.ok = TRUE)
  checkmate::assert_list(metadata_lineage_columns, null.ok = TRUE)
  checkmate::assert_list(metadata_lineage_pipeline, null.ok = TRUE)
  checkmate::assert_list(metadata_extensions, null.ok = TRUE)

  output_folder <- normalizePath(output_folder, mustWork = FALSE)
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  metadata_path <- file.path(output_folder, imd_files()$metadata)
  annotations_path <- file.path(output_folder, imd_files()$annotations)

  metadata_json <- build_write_metadata_json(
    idata = idata,
    producer_function = producer_function,
    metadata_lineage_inputs = metadata_lineage_inputs,
    metadata_lineage_args = metadata_lineage_args,
    metadata_lineage_columns = metadata_lineage_columns,
    metadata_lineage_pipeline = metadata_lineage_pipeline,
    metadata_extensions = metadata_extensions
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
  jsonlite::write_json(metadata_json, metadata_path, null = "null", auto_unbox = TRUE)

  cli::cli_alert_success("ImmunData files saved to [{output_folder}]")

  invisible(read_immundata(output_folder))
}
