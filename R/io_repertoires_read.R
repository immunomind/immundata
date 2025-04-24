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
#'   automatically detected and handled. The files can be archived via GZIP, no need to unarchive them.
#'   Pass `"<metadata>"` to read files from `"filename"` column from the input metadata table.
#' @param schema
#'   Character vector defining which columns in the input data should be used to
#'   identify unique receptor signatures. For example, `c("cdr3_aa", "v_call")`.
#' @param metadata
#'   An optional data frame containing additional metadata to merge into the annotation table.
#'   Default is `NULL`. See [read_metadata()] for more information.
#' @param barcode_col
#'   An optional character string specifying the column in the input data that represents
#'   cell ids or barcodes or other unique identifiers. Default is `NULL`.
#' @param count_col
#'   An optional character string specifying the column in the input data that stores
#'   bulk receptor counts. Default is `NULL`.
#' @param output_folder
#'   Character string specifying the directory to save the resulting Parquet files. If `NULL`,
#'   a folder named `immundata-<basename_of_path>` is created in the same directory as `path`.
#' @param enforce_schema
#'   Logical. If `TRUE`, column names and types must strictly match between files. If `FALSE`,
#'   columns are unioned
#' @param exclude_columns
#'   Character. Vector of columns to exclude from the data. Default is AIRR standard columns `*_cigar` and `*_alignment`.
#' @param repertoire_schema
#'   An optional character vector defining how annotations should be grouped into repertoires
#'   (for example, `c("sample", "donor")`). Currently unused in this function, but reserved
#'   for future expansions. Default is `NULL`.
#' @param rename_columns
#'   An optional character vector or a function that returns name of the columns to rename in the `dplyr`format:
#'   `c(new_name = "old_name")`.
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
#' @seealso [read_metadata()], [read_immundata()], [ImmunData]
#'
#' @export
read_repertoires <- function(path,
                             schema,
                             metadata = NULL,
                             barcode_col = NULL,
                             count_col = NULL,
                             enforce_schema = TRUE,
                             metadata_file_col = "File",
                             output_folder = NULL,
                             preprocess = make_default_preprocessing(),
                             postprocess = list(
                               prefix_barcodes = make_barcode_prefix()
                             ),
                             rename_columns = imd_rename_cols("10x"),
                             repertoire_schema = NULL) {
  start_time <- Sys.time()

  checkmate::assert_character(path)
  checkmate::assert_character(schema)
  checkmate::assert_data_frame(metadata, null.ok = T)
  checkmate::assert_character(metadata_file_col, null.ok = T)
  checkmate::assert_character(
    barcode_col,
    min.len = 1,
    max.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_character(count_col,
    max.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_character(output_folder,
    max.len = 1,
    null.ok = TRUE
  )
  checkmate::assert(
    checkmate::test_character(repertoire_schema,
      null.ok = TRUE
    ),
    checkmate::test_function(repertoire_schema)
  )
  checkmate::test_character(rename_columns, null.ok = TRUE)
  checkmate::assert_logical(enforce_schema)
  checkmate::assert_list(preprocess, null.ok = TRUE)
  if (!is.null(preprocess)) {
    sapply(preprocess, checkmate::assert_function)
  }

  #
  # Preprocessing the metadata
  #
  # TODO: define "<metadata>" in globals.R
  immundata_filename_col <- IMD_GLOBALS$schema$filename

  if (path[1] == "<metadata>") {
    if (!is.null(metadata)) {
      path <- normalizePath(metadata[[metadata_file_col]])
      metadata[[immundata_filename_col]] <- path
    } else {
      cli::cli_abort("Passed `<metadata>`, but no `metadata` table provided. Please provide either a list of file paths or a metadata table.")
    }
  } else {
    path <- normalizePath(Sys.glob(path), mustWork = FALSE)
  }
  checkmate::assert_file_exists(path)

  # Read the dataset
  cli::cli_h3("Reading repertoire data")
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
      prudence = "stingy",
      options = list(
        filename = TRUE,
        union_by_name = !enforce_schema
      )
    ),
    tsv = read_csv_duckdb(path,
      prudence = "stingy",
      options = list(
        delim = "\t",
        filename = TRUE,
        union_by_name = !enforce_schema
      )
    )
  )

  # Rename columns
  if (!is.null(rename_columns)) {
    cli::cli_h3("Renaming the columns and schemas")

    old_colnames <- colnames(raw_dataset)
    raw_dataset <- raw_dataset |> rename(any_of(rename_columns))
    new_colnames <- colnames(raw_dataset)
    renamed_cols <- setdiff(new_colnames, old_colnames)
    if (length(renamed_cols)) {
      cli_alert_success("Introduced new renamed columns: {renamed_cols}")
    }

    for (i in seq_along(schema)) {
      if (schema[i] %in% rename_columns) {
        schema[i] <- names(rename_columns)[schema[i] == rename_columns]
      }
    }

    if (!is.null(repertoire_schema)) {
      for (i in seq_along(repertoire_schema)) {
        if (repertoire_schema[i] %in% rename_columns) {
          repertoire_schema[i] <- names(rename_columns)[repertoire_schema[i] == rename_columns]
        }
      }
    }

    cli::cli_alert_success("Renaming is finished")
  }

  #
  # Preprocess the data
  #
  if (length(preprocess)) {
    cli::cli_h3("Preprocessing the data")

    ol <- cli::cli_ol()
    cli::cli_ol()
    for (strategy_i in seq_along(preprocess)) {
      cli::cli_li(names(preprocess)[strategy_i])
      raw_dataset <- preprocess[[strategy_i]](raw_dataset, metadata = metadata)
    }
    cli::cli_end()
    cli::cli_end(ol)

    cli::cli_alert_success("Preprocessing is finished")
  }

  # Check if we can aggregate receptors by the columns
  receptor_cols_existence <- setdiff(schema, colnames(raw_dataset))
  if (length(receptor_cols_existence) != 0) {
    cli::cli_abort("Not all columns in the receptor schema present in the data: [{receptor_cols_existence}]. Please double check and run again.")
  }

  #
  # Aggregate the data
  #
  cli::cli_h3("Aggregating the data to receptors")

  immundata_barcode_col <- imd_schema("barcode")
  immundata_receptor_id_col <- imd_schema("receptor")
  immundata_chain_id_col <- imd_schema("chain")
  immundata_count_col <- imd_schema("count")
  immundata_chain_count <- imd_schema("chain_count")

  #
  # 1) Case #1: simple receptor table - no barcodes, no count column
  #
  if (is.null(barcode_col) && is.null(count_col)) {
    raw_dataset <- raw_dataset |>
      mutate(
        {{ immundata_barcode_col }} := row_number(),
        {{ immundata_chain_id_col }} := !!rlang::sym(immundata_barcode_col)
      )

    receptor_data <- raw_dataset |>
      summarise(.by = all_of(schema)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- receptor_data |>
      full_join(raw_dataset, by = schema) |>
      mutate(
        {{ immundata_chain_count }} := 1,
        {{ immundata_count_col }} := 0
      )
  }

  #
  # 2) Case 2: bulk data - no barcodes, but with the count column
  #
  else if (is.null(barcode_col) && !is.null(count_col)) {
    raw_dataset <- raw_dataset |>
      mutate(
        {{ immundata_barcode_col }} := row_number(),
        {{ immundata_chain_id_col }} := !!rlang::sym(immundata_barcode_col)
      )

    receptor_data <- raw_dataset |>
      summarise(.by = all_of(schema)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- receptor_data |>
      full_join(raw_dataset, by = schema) |>
      mutate(
        {{ immundata_chain_count }} := !!rlang::sym(count_col),
        {{ immundata_count_col }} := 0
      )
  }

  #
  # 3) Case 3: single-cell data - barcodes, no counts
  #
  else if (!is.null(barcode_col) && is.null(count_col)) {
    raw_dataset <- raw_dataset |>
      mutate(
        {{ immundata_barcode_col }} := !!rlang::sym(barcode_col),
        {{ immundata_chain_id_col }} := row_number()
      )

    receptor_data <- raw_dataset |>
      summarise(.by = all_of(schema)) |>
      mutate(
        {{ immundata_receptor_id_col }} := row_number()
      )

    annotation_data <- receptor_data |>
      full_join(raw_dataset, by = schema) |>
      mutate({{ immundata_chain_count }} := 1, {{ immundata_count_col }} := 0)
  } else {
    #
    #  4) Something weird is happening...
    #
    cli_abort("Undefined case: passed column names for both cell identifiers and receptor counts.")
  }

  cli::cli_alert_success("Receptor data is aggregated and the annotation table is ready")

  #
  # Joining with the metadata table
  #
  if (!is.null(metadata)) {
    if (!immundata_filename_col %in% colnames(metadata)) {
      cli::cli_abort("No '{immundata_filename_col}' in the metadata table. It is imperative to have this column - `immundata` uses it to annotate the AIRR files")
    }

    cli::cli_h3("Joining the metadata table with the dataset using '{immundata_filename_col}' column")

    metadata_duckdb <- duckdb_tibble(metadata)

    annotation_data <- annotation_data |>
      left_join(metadata_duckdb, by = immundata_filename_col)

    cli::cli_alert_success("Joining is finished")
  }

  #
  # Postprocess the data
  #
  if (length(postprocess)) {
    cli::cli_h3("Postprocessing the data")

    ol <- cli::cli_ol()
    cli::cli_ol()
    for (strategy_i in seq_along(postprocess)) {
      cli::cli_li(names(postprocess)[strategy_i])
      annotation_data <- postprocess[[strategy_i]](annotation_data)
    }
    cli::cli_end()
    cli::cli_end(ol)

    cli::cli_alert_success("Postprocessing is finished")
  }

  idata <- ImmunData$new(
    schema = schema,
    annotations = annotation_data,
  )

  if (is.null(output_folder)) {
    base <- basename(path[1])
    name <- tools::file_path_sans_ext(base)
    output_folder <- file.path(dirname(path[1]), paste0("immundata-", name))
  }
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  #
  # Save the created ImmunData on disk
  #
  cli::cli_h3("Saving the newly created ImmunData to disk")

  write_immundata(idata, output_folder)

  #
  # ... and load it again so the source will be fast Parquet files
  #
  idata <- read_immundata(output_folder)

  #
  # Create repertoires
  #
  if (!is.null(repertoire_schema)) {
    cli::cli_h3("Aggregating repertoires...")
    idata <- agg_repertoires(idata, repertoire_schema)
    cli_alert_success("Aggregation is finished")
  }
  # TODO: we need to create repertoires ->
  # without repertoire aggregating (!) write it on disk with (!!) the repertoire schema

  cli::cli_h3("Summary")
  final_time <- format(round(Sys.time() - start_time, 2))
  cli_alert_info("Time elapsed: {.emph {final_time}}")

  cli_alert_success("Loaded ImmunData with the receptor schema: [{schema}]")

  if (!is.null(repertoire_schema)) {
    cli_alert_success("Loaded ImmunData with the repertoire schema: [{repertoire_schema}]")
  }

  idata
}
