#' @importFrom readr read_delim
#' @importFrom checkmate test_data_frame test_file_exists
#' @importFrom cli cli_alert_warning
#'
#' @param metadata Either a path to the file with a metadata table, or a data frame with metadata.
#' @param ... Other parameters passed to `readr::read_delim`
#'
#' @export
load_metadata <- function(metadata, filename_col = "File", delim = "\t", ...) {

  if (!test_data_frame(metadata) && !test_file_exists(metadata)) {
    cli_abort("Error in metadata: the input metadata should be either a data frame or an existing file.")
  }

  # Get the metadata table
  metadata_source <- NA
  if (test_file_exists(metadata)) {
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

  print(metadata_table)

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

  metadata_table
}
