#' @title Read and process immune repertoire files to immundata
#'
#' @description
#' This is the main function for reading immune repertoire data into the
#' `immundata` framework. It reads one or more repertoire files (AIRR TSV,
#' 10X CSV, Parquet), performs optional preprocessing and column renaming,
#' aggregates sequences into receptors based on a provided schema, optionally
#' joins external metadata, performs optional postprocessing, and returns
#' an `ImmunData` object.
#'
#' The function handles different data types (bulk, single-cell) based on
#' the presence of `barcode_col` and `count_col`. For efficiency with large
#' datasets, it processes the data and saves intermediate results (annotations)
#' as a Parquet file before loading them back into the final `ImmunData` object.
#'
#' @param path Character vector. Path(s) to input repertoire files (e.g.,
#'   `"/path/to/data/*.tsv.gz"`). Supports glob patterns via [Sys.glob()].
#'   Files can be Parquet, CSV, TSV, or gzipped versions thereof. All files
#'   must be of the same type.
#'   Alternatively, pass the special string `"<metadata>"` to read file paths
#'   from the `metadata` table (see `metadata` and `metadata_file_col` params).
#' @param schema Defines how unique receptors are identified. Can be:
#'   - A character vector of column names (e.g., `c("v_call", "j_call", "junction_aa")`).
#'   - A schema object created by [make_receptor_schema()], allowing specification
#'     of chains for pairing (e.g., `make_receptor_schema(features = c("v_call", "junction_aa"), chains = c("TRA", "TRB"))`).
#' @param metadata Optional. A data frame containing
#'   metadata to be joined with the repertoire data, read by
#'   [read_metadata()] function. If `path = "<metadata>"`, this table *must*
#'   be provided and contain the file paths column specified by `metadata_file_col`.
#'   Default: `NULL`.
#' @param barcode_col Character(1). Name of the column containing cell barcodes
#'   or other unique cell/clone identifiers for single-cell data. Triggers
#'   single-cell processing logic in [agg_receptors()]. Default: `NULL`.
#' @param count_col Character(1). Name of the column containing UMI counts or
#'   frequency counts for bulk sequencing data. Triggers bulk processing logic
#'   in [agg_receptors()]. Default: `NULL`. Cannot be specified if `barcode_col` is also
#'   specified.
#' @param locus_col Character(1). Name of the column specifying the receptor chain
#'   locus (e.g., "TRA", "TRB", "IGH", "IGK", "IGL"). Required if `schema`
#'   specifies chains for pairing. Default: `NULL`.
#' @param umi_col Character(1). Name of the column containing UMI counts for
#'   single-cell data. Used during paired-chain processing to select the most
#'   abundant chain per barcode per locus. Default: `NULL`.
#' @param preprocess List. A named list of functions to apply sequentially to the
#'   raw data *before* receptor aggregation. Each function should accept a
#'   data frame (or duckplyr_df) as its first argument. See
#'   [make_default_preprocessing()] for examples.
#'   Default: `make_default_preprocessing()`. Set to `NULL` or `list()` to disable.
#' @param postprocess List. A named list of functions to apply sequentially to the
#'   annotation data *after* receptor aggregation and metadata joining. Each
#'   function should accept a data frame (or duckplyr_df) as its first argument.
#'   See [make_default_postprocessing()] for examples.
#'   Default: `make_default_postprocessing()`. Set to `NULL` or `list()` to disable.
#' @param rename_columns Named character vector. Optional mapping to rename columns
#'   in the input files using `dplyr::rename()` syntax (e.g.,
#'   `c(new_name = "old_name", barcode = "cell_id")`). Renaming happens *before*
#'   preprocessing and schema application. See [imd_rename_cols()] for presets.
#'   Default: `imd_rename_cols("10x")`.
#' @param enforce_schema Logical(1). If `TRUE` (default), reading multiple files
#'   requires them to have the exact same columns and types. If `FALSE`, columns
#'   are unioned across files (potentially slower, requires more memory).
#'   Default: `TRUE`.
#' @param metadata_file_col Character(1). The name of the column in the `metadata`
#'   table that contains the full paths to the repertoire files. Only used when
#'   `path = "<metadata>"`. Default: `"File"`.
#' @param output_folder Character(1). Path to a directory where intermediate
#'   processed annotation data will be saved as `annotations.parquet` and
#'   `metadata.json`. If `NULL` (default), a folder named
#'   `immundata-<basename_without_ext>` is created in the same directory as the
#'   first input file specified in `path`. The final `ImmunData` object reads
#'   from these saved files. Default: `NULL`.
#' @param repertoire_schema Character vector or Function. Defines columns used to
#'   group annotations into distinct repertoires (e.g., by sample or donor).
#'   If provided, [agg_repertoires()] is called after loading to add repertoire-level
#'   summaries and metrics. Default: `NULL`.
#'
#' @details
#' The function executes the following steps:
#' 1.  Validates inputs.
#' 2.  Determines the list of input files based on `path` and `metadata`. Checks file extensions.
#' 3.  Reads data using `duckplyr` (`read_parquet_duckdb` or `read_csv_duckdb`). Handles `.gz`.
#' 4.  Applies column renaming if `rename_columns` is provided.
#' 5.  Applies preprocessing steps sequentially if `preprocess` is provided.
#' 6.  Aggregates sequences into receptors using [agg_receptors()], based on `schema`, `barcode_col`, `count_col`, `locus_col`, and `umi_col`. This creates the core annotation table.
#' 7.  Joins the `metadata` table if provided.
#' 8.  Applies postprocessing steps sequentially if `postprocess` is provided.
#' 9.  Creates a temporary `ImmunData` object in memory.
#' 10. Determines the `output_folder` path.
#' 11. Saves the processed annotation table and metadata using [write_immundata()] to the `output_folder`.
#' 12. Loads the data back from the saved Parquet files using [read_immundata()] to create the final `ImmunData` object. This ensures the returned object is backed by efficient storage.
#' 13. If `repertoire_schema` is provided, calls [agg_repertoires()] on the loaded object to define and summarize repertoires.
#' 14. Returns the final `ImmunData` object.
#'
#' @return An `ImmunData` object containing the processed receptor annotations.
#'   If `repertoire_schema` was provided, the object will also contain repertoire
#'   definitions and summaries calculated by [agg_repertoires()].
#'
#' @seealso [ImmunData], [read_immundata()], [write_immundata()], [read_metadata()],
#'   [agg_receptors()], [agg_repertoires()], [make_receptor_schema()],
#'   [make_default_preprocessing()], [make_default_postprocessing()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #
#' # Example 1: single-chain, one file
#' #
#' # Read a single AIRR TSV file, defining receptors by V/J/CDR3_aa
#' # Assume "my_sample.tsv" exists and follows AIRR format
#'
#' # Create a dummy file for illustration
#' airr_data <- data.frame(
#'   sequence_id = paste0("seq", 1:5),
#'   v_call = c("TRBV1", "TRBV1", "TRBV2", "TRBV1", "TRBV3"),
#'   j_call = c("TRBJ1", "TRBJ1", "TRBJ2", "TRBJ1", "TRBJ1"),
#'   junction_aa = c("CASSL...", "CASSL...", "CASSD...", "CASSL...", "CASSF..."),
#'   productive = c(TRUE, TRUE, TRUE, FALSE, TRUE),
#'   locus = c("TRB", "TRB", "TRB", "TRB", "TRB")
#' )
#' readr::write_tsv(airr_data, "my_sample.tsv")
#'
#' # Define receptor schema
#' receptor_def <- c("v_call", "j_call", "junction_aa")
#'
#' # Specify output folder
#' out_dir <- tempfile("immundata_output_")
#'
#' # Read the data (disabling default preprocessing for this simple example)
#' idata <- read_repertoires(
#'   path = "my_sample.tsv",
#'   schema = receptor_def,
#'   output_folder = out_dir,
#'   preprocess = NULL, # Disable default productive filter for demo
#'   postprocess = NULL # Disable default barcode prefixing
#' )
#'
#' print(idata)
#' print(idata$annotations)
#'
#' #
#' # Example 2: single-chain, multiple files
#' #
#' # Read multiple files using metadata
#' # Create dummy files and metadata
#' readr::write_tsv(airr_data[1:2, ], "sample1.tsv")
#' readr::write_tsv(airr_data[3:5, ], "sample2.tsv")
#' meta <- data.frame(
#'   SampleID = c("S1", "S2"),
#'   Tissue = c("PBMC", "Tumor"),
#'   FilePath = c(normalizePath("sample1.tsv"), normalizePath("sample2.tsv"))
#' )
#' readr::write_tsv(meta, "metadata.tsv")
#'
#' idata_multi <- read_repertoires(
#'   path = "<metadata>",
#'   metadata = meta,
#'   metadata_file_col = "FilePath",
#'   schema = receptor_def,
#'   repertoire_schema = "SampleID", # Aggregate by SampleID
#'   output_folder = tempfile("immundata_multi_"),
#'   preprocess = make_default_preprocessing("airr"), # Use default AIRR filters
#'   postprocess = NULL
#' )
#'
#' print(idata_multi)
#' print(idata_multi$repertoires) # Check repertoire summary
#'
#' # Clean up dummy files
#' file.remove("my_sample.tsv", "sample1.tsv", "sample2.tsv", "metadata.tsv")
#' unlink(out_dir, recursive = TRUE)
#' unlink(attr(idata_multi, "output_folder"), recursive = TRUE) # Get path used by function
#' }
read_repertoires <- function(path,
                             schema,
                             metadata = NULL,
                             barcode_col = NULL,
                             count_col = NULL,
                             locus_col = NULL,
                             umi_col = NULL,
                             preprocess = make_default_preprocessing(),
                             postprocess = make_default_postprocessing(),
                             rename_columns = imd_rename_cols("10x"),
                             enforce_schema = TRUE,
                             metadata_file_col = "File",
                             output_folder = NULL,
                             repertoire_schema = NULL) {
  start_time <- Sys.time()

  checkmate::assert_character(path)

  if (checkmate::test_character(schema)) {
    schema <- make_receptor_schema(features = schema, chains = NULL)
  }

  assert_receptor_schema(schema)

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
  checkmate::assert_character(locus_col,
    min.len = 1,
    max.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_character(umi_col,
    min.len = 1,
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
  checkmate::assert_character(rename_columns, null.ok = TRUE)
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


  #
  # Aggregate the data
  #
  cli::cli_h3("Aggregating the data to receptors")

  annotation_data <- agg_receptors(
    dataset = raw_dataset,
    schema = schema,
    barcode_col = barcode_col,
    count_col = count_col,
    locus_col = locus_col,
    umi_col = umi_col
  )

  cli::cli_alert_success("Execution plan for receptor data aggregation and annotation is ready")

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
  idata <- read_immundata(output_folder, verbose = FALSE)

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

  idata_size <- idata |>
    count() |>
    pull("n")

  cli_alert_success("Loaded ImmunData with the receptor schema: [{schema}]")

  if (!is.null(repertoire_schema)) {
    cli_alert_success("Loaded ImmunData with the repertoire schema: [{repertoire_schema}]")
  }

  if (idata_size == 0) {
    cli_alert_warning("Loaded ImmunData with zero (!) chains. Possible problems: wrong {.code 'chain'} specification to the receptor schema (e.g., {.code 'TCRB'} instead of {.code 'TRB'}), or preproces/postprocess filters")
  } else {
    cli_alert_success("Loaded ImmunData with [{idata_size}] chains")
  }

  idata
}
