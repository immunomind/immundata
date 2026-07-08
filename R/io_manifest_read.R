#' @title Load and Validate a Manifest for Immune Repertoire Files
#'
#' @description
#' This function loads a manifest from either a file path or a data frame,
#' validates the presence of a column with repertoire file paths, and converts
#' all file paths to absolute paths. It is used to support flexible pipelines
#' for loading bulk or single-cell immune repertoire data across samples.
#'
#' If the input is a file path, the function reads it with `readr::read_delim`.
#' If the input is a data frame, it checks whether file paths are absolute;
#' relative paths are only allowed when the manifest is loaded from a file.
#'
#' It warns the user if many of the files listed in the manifest are missing,
#' and stops execution if none of the files exist.
#'
#' The column with file paths is normalized into the internal filename schema.
#'
#' @param manifest A manifest table. Can be either:
#'   - a data frame with per-file annotations,
#'   - or a path to a CSV/TSV/TXT manifest file.
#'
#' @param file_col A string specifying the name of the column in the manifest
#'   that contains paths to repertoire files. Defaults to `"file"`.
#'
#' @param delim Delimiter used to read the manifest file. If `NULL`, it is
#'   inferred from the extension: comma for `.csv`, tab for `.tsv` and `.txt`.
#'
#' @param ... Additional arguments passed to `readr::read_delim()` when reading
#'   a manifest from a file.
#'
#' @return A validated and updated manifest data frame with absolute file paths
#'   and an additional internal column named `imd_filename`.
#'
#' @concept ingestion
#' @export
read_manifest <- function(manifest, file_col = "file", delim = NULL, ...) {

  if (!checkmate::test_data_frame(manifest) && !checkmate::test_file_exists(manifest)) {
    cli_abort("Error in manifest: the input manifest should be either a data frame or an existing file.")
  }

  manifest_source <- NA
  if (checkmate::test_file_exists(manifest)) {
    manifest_basename <- tolower(basename(manifest))
    if (manifest_basename %in% c("metadata.csv", "metadata.tsv", "metadata.txt")) {
      cli_abort("Input repertoire metadata tables are now manifests. Rename [{basename(manifest)}] to [manifest.csv] and use {.fn read_manifest}. Snapshot metadata.json is not affected.")
    }

    if (is.null(delim)) {
      extension <- tolower(tools::file_ext(manifest))
      delim <- switch(extension,
        csv = ",",
        tsv = "\t",
        txt = "\t",
        cli_abort("Unknown manifest file type: [{extension}]. Supported manifest types: CSV, TSV, TXT.")
      )
    }

    manifest_table <- read_delim(manifest, delim = delim, ...)
    manifest_source <- "file" # TODO: enum
  } else {
    manifest_table <- manifest
    manifest_source <- "df"
  }

  # Check for the column
  if (!(file_col %in% colnames(manifest_table))) {
    cli_abort("Error: no column [{file_col}] with full file paths and names in the input manifest.")
  }

  # Preprocess the files:
  # - either they are in the same folder as manifest
  # - or those are full paths to the files
  manifest_table[[file_col]] <- sapply(manifest_table[[file_col]], function (path) {
    if (!grepl("^(?:[A-Za-z]:/|/)", path)) {
      # The path is not a full path, so we should add the manifest directory.
      if (manifest_source == "file") {
        path <- file.path(dirname(manifest), path)
      } else {
        cli_abort("Error: the input manifest is a data frame, but paths are relative.
                  Provide full paths in the [{file_col}] column, e.g.,
                  [/Users/username/projects/data/sample1.tsv] instead of [sample1.tsv] or [sample1]")
      }
    }

    normalizePath(path)
  })
  names(manifest_table[[file_col]]) <- NULL

  # Check how many files from the file column exist:
  file_list <- manifest_table[[file_col]]
  file_existed <- sapply(file_list, test_file_exists)
  n_existed <- sum(file_existed)
  n_threshold <- round(length(file_list) * 0.1) + 1

  cli_alert_info("Found {n_existed}/{length(file_list)} repertoire files from the manifest on disk")
  if (n_existed == 0) {
    cli_abort("Error: found zero (!) repertoire files passed in the manifest. Are the file paths in the manifest correct?")
  } else if (n_existed <= n_threshold) {
    cli_alert_warning("Warning: found only {n_existed} files out of {length(file_list)} in the manifest. Please check if you planned to work with more repertoire files. Continuing the execution.")
  }

  cli_alert_success("Manifest parsed successfully")

  immundata_filename_col <- IMD_GLOBALS$schema$manifest_filename
  manifest_table[[immundata_filename_col]] <- manifest_table[[file_col]]

  manifest_table
}
