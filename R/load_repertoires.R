#' @importFrom dplyr select group_by all_of arrange summarise mutate row_number full_join
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_ul cli_end cli_ol cli_abort
#' @importFrom duckplyr read_parquet_duckdb read_csv_duckdb compute_parquet
#' @importFrom checkmate assert_character assert_file_exists assert_directory_exists assert_data_frame
#' @export
load_repertoires <- function(path,
                             schema,
                             metadata = NULL,
                             barcode_col = NULL,
                             count_col = NULL,
                             repertoire_schema = NULL,
                             output_folder = NULL,
                             verbose = TRUE) {
  start_time <- Sys.time()

  assert_character(path)
  assert_character(schema)
  assert_data_frame(metadata)
  assert_character(
    barcode_col,
    min.len = 1,
    max.len = 1,
    null.ok = TRUE
  )
  assert_character(count_col,
    max.len = 1,
    null.ok = TRUE
  )
  assert_character(
    repertoire_schema,
    null.ok = TRUE
  )
  assert_logical(verbose)

  path <- normalizePath(Sys.glob(path), mustWork = FALSE)
  assert_file_exists(path)

  # Read the dataset
  cli_alert_info("Reading repertoire data from:")

  file_check_results <- check_file_extensions(path)
  input_file_type <- file_check_results$filetype
  delim <- file_check_results$delim

  raw_dataset <- switch(input_file_type,
    parquet = read_parquet_duckdb(path, prudence = "stingy", options = list(filename = TRUE, union_by_name = TRUE)),
    csv = read_csv_duckdb(path, options = list(filename = TRUE)),
    tsv = read_csv_duckdb(path, options = list(delim = "\t", filename = TRUE))
  )

  # Preprocess the data and aggregate receptors together
  cli_alert_info("Preprocessing and aggregating the data")

  # 1) Case #1: simple receptor table - no barcodes, no count column
  if (is.null(barcode_col) && is.null(count_col)) {
    raw_dataset <- raw_dataset |>
      mutate(barcode = row_number())

    receptor_data <- raw_dataset |>
      summarise(.by = all_of(schema)) |>
      mutate(receptor_id = row_number())

    annotation_data <- receptor_data |> full_join(raw_dataset, by = schema)
  }

  # 2) Case 2: bulk data - no barcodes, but with the count column
  else if (is.null(barcode_col) && !is.null(count_col)) {
    cli_abort("Not supported yet")
  }

  # 3) Case 3: single-cell data - barcodes, no counts
  else if (!is.null(barcode_col) && is.null(count_col)) {
    cli_abort("Not supported yet")
  } else {
    cli_abort("Undefined case: passed column names for both barcodes and receptor counts.")
  }

  # Combine annotations and the metadata file
  # TODO

  # Create the output folder
  if (!is.null(output_folder)) {
    output_folder <- normalizePath(output_folder, mustWork = FALSE)
  } else {
    base <- basename(path[1])
    name <- tools::file_path_sans_ext(base)
    output_folder <- file.path(dirname(path[1]), paste0("immundata-", name))
  }
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  receptor_path <- file.path(output_folder, imd_files()$receptors)
  annotations_path <- file.path(output_folder, imd_files()$annotations)

  cli_alert_info("Writing the receptor data to [{receptor_path}]")
  compute_parquet(receptor_data,
    receptor_path,
    options = list(
      compression = "zstd",
      compression_level = 9
    )
  )

  cli_alert_info("Writing the annotation data to [{annotations_path}]")
  compute_parquet(annotation_data,
    annotations_path,
    options = list(
      compression = "zstd",
      compression_level = 9
    )
  )

  final_time <- format(round(Sys.time() - start_time, 2))
  cli_alert_info("Time elapsed: {.emph {final_time}}")

  cli_alert_success("ImmunData files saved to [{output_folder}]")

  # Read them back to create the resultant ImmunData
  load_immundata(output_folder, verbose)
}
