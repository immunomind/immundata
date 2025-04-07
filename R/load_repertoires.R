#' @importFrom dplyr select group_by all_of arrange summarise mutate row_number full_join
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_ul cli_end cli_ol cli_abort
#' @importFrom duckplyr read_parquet_duckdb read_csv_duckdb compute_parquet
#' @importFrom checkmate assert_character assert_file_exists assert_directory_exists
#' @export
load_repertoires <- function(path,
                             schema,
                             metadata = NULL,
                             barcode_col = NULL,
                             count_col = NULL,
                             repertoire_schema = NULL,
                             output_folder = NULL,
                             verbose = TRUE) {
  assert_character(path)
  assert_character(schema)
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
    stop()
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

  cli_alert_success("ImmunData files saved to [{output_folder}]")

  # Read them back to create the resultant ImmunData
  load_immundata(output_folder, schema, verbose)
}

# DECISIONS
# 1) parse the list of files / from glob; including CSV/TSV files - thanks to duckdb

# IMMUNDATA
# 1) filter_receptors
# 2) filter_annotations
# 3) filter_barcodes
# 4) filter_repertoires (the problem here is that immundata has its own repertoire_id column)
# 5) select_repertoire via [[...]]

# OPTIONS
#     1) temporary file/folder for duckdb
#     2.1) temporary file/folder for future ImmunData caches.
#     2.2) cache_policy: c("none", "immundata", "all")
#     3) optimize output parquet files
# --  4.1) parse multiple files
# **  4.2) parse metadata
#     5) add an option to not create the intermediate immundata
#     6) verbose level
# **  7) split by locus to different files
# *** 8) bulk case with counts
# *** 9) single-cell case with barcodes
# *   10) how would look an error in case of different schemas across files?
# --  11.1) parse csv/tsv files
# *   11.2) test it (:


# CASES
# ** 1) working with metadata bullshit
# ** 2) multiple locus in the same file - what to do? Locus column in general
#    3) multiple chains/contigs merging strategy for TCRs (errors)?
#     - create a separate immundata with all this shit
#     - Maybe we can do the same for productive data
#     - should have an option to turn it on/off ("off" by default, because screw that)
#    4) how to work with multiple light chains in BCRs (not errors)?
#     - ???
# ** 5) filtering strategy - productive / non productive
#     - filter_nonproductive (or just pass a list of strategies)
#    6) how to process gene segments / families
#     - create families
