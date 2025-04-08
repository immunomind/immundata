#' @importFrom dplyr select group_by all_of any_of arrange summarise mutate row_number full_join
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_ul cli_end cli_ol cli_abort
#' @importFrom duckplyr read_parquet_duckdb read_csv_duckdb compute_parquet duckdb_tibble
#' @importFrom checkmate assert_character assert_logical assert_file_exists assert_directory_exists assert_data_frame
#' @importFrom rlang sym
#'
#' @title Read Immune Receptor Repertoire Data and Create ImmunData
#'
#' @description
#' This function ingests a repertoire dataset (Parquet, CSV, or TSV), aggregates receptors
#' based on a user-defined schema, and splits the result into receptor-level and annotation-level
#' tables. The resulting data is saved to a designated output folder as two Parquet files
#' (receptors and annotations) and then reloaded to create an `ImmunData` object.
#'
#' @param path
#'   Path to an input file. This file may be Parquet, CSV, or TSV. The file extension is
#'   automatically detected and handled.
#' @param schema
#'   Character vector defining which columns in the input data should be used to
#'   identify unique receptor signatures. For example, `c("V_gene", "J_gene", "CDR3_nt")`.
#' @param metadata
#'   An optional data frame containing additional metadata to merge into the annotation table.
#'   Default is `NULL`.
#' @param barcode_col
#'   An optional character string specifying the column in the input data that represents
#'   cell barcodes or other unique identifiers. Default is `NULL`.
#' @param count_col
#'   An optional character string specifying the column in the input data that stores
#'   bulk receptor counts. Default is `NULL`.
#' @param repertoire_schema
#'   An optional character vector defining how annotations should be grouped into repertoires
#'   (for example, `c("sample", "donor")`). Currently unused in this function, but reserved
#'   for future expansions. Default is `NULL`.
#' @param output_folder
#'   Character string specifying the directory to save the resulting Parquet files. If `NULL`,
#'   a folder named `immundata-<basename_of_path>` is created in the same directory as `path`.
#' @param enforce_schema
#'   Logical. If `TRUE`, column names and types must strictly match between files. If `FALSE`,
#'   columns are unioned
#' @param verbose
#' .  Logical. Not used -- for now.
#'
#' @details
#' 1. **Reading** – The function automatically detects whether `path` points
#'    to a Parquet, CSV, or TSV file, using `read_parquet_duckdb` or `read_csv_duckdb`.
#' 2. **Aggregation** – Receptor uniqueness is determined by the columns named in
#'    `schema`, while barcodes or counts are handled depending on which parameters
#'    (`barcode_col`, `count_col`) are provided.
#' 3. **Saving** – The final receptor-level and annotation-level tables are written
#'    to Parquet files in `output_folder`.
#' 4. **Reloading** – The function calls [read_immundata()] on the newly
#'    created folder to return a fully instantiated `ImmunData`.
#'
#' @seealso [read_immundata()], [ImmunData]
#'
#' @export
read_repertoires <- function(path,
                             schema,
                             metadata = NULL,
                             barcode_col = NULL,
                             count_col = NULL,
                             enforce_schema = TRUE,
                             exclude_columns = imd_drop_cols()$airr,
                             output_folder = NULL,
                             repertoire_schema = NULL,
                             verbose = TRUE) {
  start_time <- Sys.time()

  assert_character(path)
  assert_character(schema)
  if (!is.null(metadata)) {
    assert_data_frame(metadata)
  }
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
  assert_character(
    exclude_columns,
    null.ok = TRUE
  )
  assert_logical(verbose)
  assert_logical(enforce_schema)

  path <- normalizePath(Sys.glob(path), mustWork = FALSE)
  assert_file_exists(path)

  # Read the dataset
  cli_alert_info("Reading repertoire data from:")

  file_check_results <- check_file_extensions(path)
  input_file_type <- file_check_results$filetype
  delim <- file_check_results$delim

  raw_dataset <- switch(input_file_type,
    parquet = read_parquet_duckdb(path,
      prudence = "stingy",
      options = list(
        filename = TRUE,
        union_by_name = !enforce_schema
      )
    ),
    csv = read_csv_duckdb(path,
      options = list(
        filename = TRUE,
        union_by_name = !enforce_schema
      )
    ),
    tsv = read_csv_duckdb(path,
      options = list(
        delim = "\t",
        filename = TRUE,
        union_by_name = !enforce_schema
      )
    )
  )

  # Preprocess the data and aggregate receptors together
  cli_alert_info("Preprocessing and aggregating the data")

  immundata_barcode_col <- IMD_GLOBALS$schema$barcode
  immundata_receptor_id_col <- IMD_GLOBALS$schema$receptor
  immundata_count_col <- IMD_GLOBALS$schema$count

  if (!is.null(exclude_columns)) {
    raw_dataset <- raw_dataset |>
      select(-any_of(exclude_columns))
  }

  # 1) Case #1: simple receptor table - no barcodes, no count column
  if (is.null(barcode_col) && is.null(count_col)) {
    raw_dataset <- raw_dataset |>
      mutate({{ immundata_barcode_col }} := row_number())

    receptor_data <- raw_dataset |>
      summarise(.by = all_of(schema)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- receptor_data |>
      full_join(raw_dataset, by = schema) |>
      mutate({{ immundata_count_col }} := 1)
  }

  # 2) Case 2: bulk data - no barcodes, but with the count column
  else if (is.null(barcode_col) && !is.null(count_col)) {
    raw_dataset <- raw_dataset |>
      mutate({{ immundata_barcode_col }} := row_number())

    receptor_data <- raw_dataset |>
      summarise(.by = all_of(schema)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- receptor_data |>
      full_join(raw_dataset, by = schema) |>
      mutate({{ immundata_count_col }} := !!rlang::sym(count_col))
  }

  # 3) Case 3: single-cell data - barcodes, no counts
  else if (!is.null(barcode_col) && is.null(count_col)) {
    raw_dataset <- raw_dataset |>
      mutate({{ immundata_barcode_col }} := !!rlang::sym(barcode_col))

    receptor_data <- raw_dataset |>
      summarise(.by = all_of(schema)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- receptor_data |>
      full_join(raw_dataset, by = schema) |>
      mutate({{ immundata_count_col }} := 1)
  } else {
    cli_abort("Undefined case: passed column names for both barcodes and receptor counts.")
  }

  # Combine annotations and the metadata file
  if (!is.null(metadata)) {
    immundata_filename_col <- IMD_GLOBALS$schema$filename

    metadata_duckdb <- duckdb_tibble(metadata)

    annotation_data <- annotation_data |>
      full_join(metadata_duckdb, by = immundata_filename_col)
  }

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
  read_immundata(output_folder, verbose)
}
