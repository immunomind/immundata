check_file_extensions <- function(path, verbose = TRUE) {

  ol <- cli_ol()
  cli_ol(path)
  cli_end(ol)

  cli_alert_info("Checking if all files are of the same type")

  input_file_type <- NA
  delim <- NA

  unique_extensions <- file_ext(path) |>
    unique() |>
    tolower()

  if (length(unique_extensions) == 1) {

    if (unique_extensions %in% c("gz", "gzip")) {
      unique_extensions <- strsplit(path[1], ".", fixed = TRUE)[[1]]
      unique_extensions <- paste(tail(unique_extensions, 2), collapse = ".")
    }

    # TODO: I have no idea how to make it more elegant.
    # TODO: make enum-like list for file types
    if (unique_extensions %in% c("parquet", "csv", "tsv", "csv.gz", "tsv.gz", "csv.gzip", "tsv.gzip")) {
      input_file_type <- strsplit(unique_extensions, ".", fixed = TRUE)[[1]][1]

      if (input_file_type == "tsv") {
        delim <- "\t"
      }
    } else {
      cli_abort("Unknown file type: [{unique_extensions}]. Supported file types: Parquet, CSV, TSV, gzipped CSV and TSV")
    }
    cli_alert_success("All files have the same extension")
  } else {
    cli_abort("Not all files of the same type. Please convert them all to the same type, and try again")
  }

  list(filetype = input_file_type, delim = delim)
}
