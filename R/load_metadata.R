#' @importFrom readr read_delim
#' @export
load_metadata <- function(path, filename_col = "File", delim = "\t", ...) {
  # Check if the path even exists
  metadata_table <- read_delim(path, delim = delim, ...)
  # Check if the `filename_col` exists
  # Check for the existence of files in the `filename_col`
}
