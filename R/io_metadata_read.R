#' @title Load and Validate Metadata Table for Immune Repertoire Files
#'
#' @description
#' This function loads a metadata table from either a file path or a data frame,
#' validates the presence of a column with repertoire file paths, and converts all
#' file paths to absolute paths. It is used to support flexible pipelines for
#' loading bulk or single-cell immune repertoire data across samples.
#'
#' If the input is a file path, the function attempts to read it with `readr::read_delim`.
#' If the input is a data frame, it checks whether file paths are absolute;
#' relative paths are only allowed when metadata is loaded from a file.
#'
#' It warns the user if many of the files listed in the metadata table are missing,
#' and stops execution if none of the files exist.
#'
#' The column with file paths is normalized and renamed to match the internal filename schema.
#'
#' @param metadata A metadata table. Can be either:
#'   - a data frame with metadata,
#'   - or a path to a text/TSV/CSV file that can be read with `readr::read_delim`.
#'
#' @param filename_col A string specifying the name of the column in the metadata table
#'   that contains paths to repertoire files. Defaults to `"File"`.
#'
#' @param delim Delimiter used to read the metadata file (if a path is provided). Defaults to `"\t"`.
#'
#' @param ... Additional arguments passed to `readr::read_delim()` when reading metadata from a file.
#'
#' @return A validated and updated metadata data frame with absolute file paths,
#'   and an additional column renamed according to `IMD_GLOBALS$schema$filename`.
#'
#' @concept ingestion
#' @export
read_metadata <- function(metadata, filename_col = "File", delim = "\t", ...) {

  if (!checkmate::test_data_frame(metadata) && !checkmate::test_file_exists(metadata)) {
    cli_abort("Error in metadata: the input metadata should be either a data frame or an existing file.")
  }

  # Get the metadata table
  metadata_source <- NA
  if (checkmate::test_file_exists(metadata)) {
    metadata_table <- read_delim(metadata, delim = delim, ...)
    metadata_source <- "file" # TODO: enum
  } else {
    metadata_table <- metadata
    metadata_source <- "df"
  }

  # Check for the column
  if (!(filename_col %in% colnames(metadata_table))) {
    cli_abort("Error: no column [{filename_col}] with full file paths and names in the input metadata table.")
  }

  # Preprocess the files:
  # - either they are in the same folder as metadata
  # - or those are full paths to the files
  metadata_table[[filename_col]] <- sapply(metadata_table[[filename_col]], function (path) {
    if (!grepl("^(?:[A-Za-z]:/|/)", path)) {
      # The path is not a full path, so we should add the directory of metadata to it - if it's a file
      if (metadata_source == "file") {
        path <- file.path(dirname(metadata), path)
      } else {
        cli_abort("Error: the input metadata is a data frame, but paths are relative.
                  Provide full paths in the [{filename_col}] column, e.g.,
                  [/Users/username/projects/data/sample1.tsv] instead of [sample1.tsv] or [sample1]")
      }
    }

    normalizePath(path)
  })
  names(metadata_table[[filename_col]]) <- NULL

  # Check how many files from the file column exist:
  file_list <- metadata_table[[filename_col]]
  file_existed <- sapply(file_list, test_file_exists)
  n_existed <- sum(file_existed)
  n_threshold <- round(length(file_list) * 0.1) + 1

  cli_alert_info("Found {n_existed}/{length(file_list)} repertoire files from the metadata on the disk")
  if (n_existed == 0) {
    cli_abort("Error: found zero (!) repertoire files, passed in the metadata. Are the file paths in the metadata correct?")
  } else if (n_existed <= n_threshold) {
    cli_alert_warning("Warning: found only {n_existed} files out of {length(file_list)} in the metadata. Please check if you planned to work with more repertoire files. Continuing the execution.")
  }

  cli_alert_success("Metadata parsed successfully")

  # Renaming columns could lead to downstream schema issues
  immundata_filename_col <- IMD_GLOBALS$schema$filename
  metadata_table[[immundata_filename_col]] <- metadata_table[[filename_col]]

  metadata_table
}
