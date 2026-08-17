#' @title Read and process immune repertoire files to immundata
#'
#' @description
#' This is the main function for reading immune repertoire data into the
#' `immundata` framework. It reads one or more repertoire files (AIRR TSV,
#' 10X CSV, Parquet), performs optional preprocessing and column renaming,
#' aggregates sequences into receptors based on a provided schema, optionally
#' joins manifest annotations, performs optional postprocessing, and returns
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
#'   Alternatively, pass the special string `"<manifest>"` to read file paths
#'   from the `manifest` table (see `manifest` and `manifest_file_col` params).
#' @param schema Defines how unique receptors are identified. Can be:
#'   - A character vector of column names (e.g., `c("v_call", "j_call", "junction_aa")`).
#'   - A schema object created by [make_receptor_schema()], allowing specification
#'     of chains for pairing (e.g., `make_receptor_schema(features = c("v_call", "junction_aa"), chains = c("TRA", "TRB"))`).
#' @param manifest Optional. A data frame containing
#'   per-file annotations to be joined with the repertoire data, read by
#'   [read_manifest()] function. If `path = "<manifest>"`, this table *must*
#'   be provided and contain the file paths column specified by `manifest_file_col`.
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
#'   single-cell data. Required when `barcode_col` is used. It is used to
#'   select the most abundant chain within a barcode (and within a locus for
#'   paired-chain schemas). Default: `NULL`.
#' @param preprocess List. A named list of functions to apply sequentially to the
#'   raw data *before* receptor aggregation. Each function should accept a
#'   data frame (or duckplyr_df) as its first argument. See
#'   [make_default_preprocessing()] for examples.
#'   Default: `make_default_preprocessing()`. Set to `NULL` or `list()` to disable.
#' @param postprocess List. A named list of functions to apply sequentially to the
#'   annotation data *after* receptor aggregation and manifest joining. Each
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
#' @param manifest_file_col Character(1). The name of the column in the `manifest`
#'   table that contains the full paths to the repertoire files. Only used when
#'   `path = "<manifest>"`. Default: `"file"`.
#' @param output_folder Character(1). Path to a directory where intermediate
#'   processed annotation data will be saved as `annotations.parquet` and
#'   `metadata.json`. If `NULL` (default), a folder named
#'   `immundata-<basename_without_ext>` is created in the same directory as the
#'   first input file specified in `path`. The final `ImmunData` object reads
#'   from these saved files. Default: `NULL`.
#' @param repertoire_schema Character vector, Function, `NULL`, or a special
#'   string. Defines columns used to group annotations into distinct repertoires
#'   (e.g., by sample or donor). `"<manifest>"` means group by input file /
#'   manifest row. `"<auto>"` chooses `"<manifest>"` behavior when
#'   `path = "<manifest>"`, otherwise it groups by the internal input filename
#'   column. If `NULL`, no repertoires are created. Default: `"<auto>"`.
#' @param verbose Logical(1). Whether to print informative messages. Defaults to
#'   `getOption("immundata.verbose", TRUE)`.
#'
#' @details
#' The function executes the following steps:
#' 1.  Validates inputs.
#' 2.  Determines the list of input files based on `path` and `manifest`. Checks file extensions.
#' 3.  Reads data using `duckplyr` (`read_parquet_duckdb` or `read_csv_duckdb`). Handles `.gz`.
#' 4.  Applies column renaming if `rename_columns` is provided.
#' 5.  Applies preprocessing steps sequentially if `preprocess` is provided.
#' 6.  Aggregates sequences into receptors using [agg_receptors()], based on `schema`, `barcode_col`, `count_col`, `locus_col`, and `umi_col`. This creates the core annotation table.
#' 7.  Joins the `manifest` table if provided.
#' 8.  Applies postprocessing steps sequentially if `postprocess` is provided.
#' 9.  Creates a temporary `ImmunData` object in memory.
#' 10. Determines the `output_folder` path.
#' 11. If `repertoire_schema` resolves to columns, calls [agg_repertoires()] to define and summarize repertoires.
#' 12. Saves the processed annotation table and metadata using [write_immundata()] to the `output_folder`.
#' 13. Loads the data back from the saved Parquet files using [read_immundata()] to create the final `ImmunData` object. This ensures the returned object is backed by efficient storage.
#' 14. Returns the final `ImmunData` object.
#'
#' @return An `ImmunData` object containing the processed receptor annotations.
#'   If `repertoire_schema` resolves to columns, the object will also contain repertoire
#'   definitions and summaries calculated by [agg_repertoires()].
#'
#' @seealso [ImmunData], [read_immundata()], [write_immundata()], [read_manifest()],
#'   [agg_receptors()], [agg_repertoires()], [make_receptor_schema()],
#'   [make_default_preprocessing()], [make_default_postprocessing()]
#'
#' @concept ingestion
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
#' # Read multiple files using a manifest
#' # Create dummy files and a manifest
#' readr::write_tsv(airr_data[1:2, ], "sample1.tsv")
#' readr::write_tsv(airr_data[3:5, ], "sample2.tsv")
#' manifest <- data.frame(
#'   SampleID = c("S1", "S2"),
#'   Tissue = c("PBMC", "Tumor"),
#'   file = c(normalizePath("sample1.tsv"), normalizePath("sample2.tsv"))
#' )
#' readr::write_csv(manifest, "manifest.csv")
#'
#' idata_multi <- read_repertoires(
#'   path = "<manifest>",
#'   manifest = manifest,
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
#' file.remove("my_sample.tsv", "sample1.tsv", "sample2.tsv", "manifest.csv")
#' unlink(out_dir, recursive = TRUE)
#' unlink(attr(idata_multi, "output_folder"), recursive = TRUE) # Get path used by function
#' }
read_repertoires <- function(path,
                             schema,
                             manifest = NULL,
                             barcode_col = NULL,
                             count_col = NULL,
                             locus_col = NULL,
                             umi_col = NULL,
                             preprocess = make_default_preprocessing(),
                             postprocess = make_default_postprocessing(),
                             rename_columns = imd_rename_cols("10x"),
                             enforce_schema = TRUE,
                             manifest_file_col = "file",
                             output_folder = NULL,
                             repertoire_schema = "<auto>",
                             verbose = getOption("immundata.verbose", TRUE)) {
  start_time <- Sys.time()

  checkmate::assert_character(path)

  if (checkmate::test_character(schema)) {
    schema <- make_receptor_schema(features = schema, chains = NULL)
  }

  assert_receptor_schema(schema)

  checkmate::assert_data_frame(manifest, null.ok = TRUE)
  checkmate::assert_character(manifest_file_col, null.ok = TRUE)
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
  checkmate::assert_flag(verbose)
  checkmate::assert_list(preprocess, null.ok = TRUE)
  if (!is.null(preprocess)) {
    sapply(preprocess, checkmate::assert_function)
  }

  requested_rename_columns <- rename_columns
  applied_rename_columns <- requested_rename_columns[0]
  missing_rename_columns <- requested_rename_columns[0]
  dropped_columns <- character()
  repertoire_schema_was_special <- is_special_repertoire_schema(repertoire_schema)

  #
  # Preprocessing the manifest
  #
  # TODO: define "<manifest>" in globals.R
  immundata_filename_col <- IMD_GLOBALS$schema$manifest_filename
  path_from_manifest <- identical(path[1], "<manifest>")

  if (path[1] == "<metadata>") {
    cli::cli_abort("Input repertoire metadata tables are now manifests. Use {.code path = '<manifest>'}, {.arg manifest}, and {.arg manifest_file_col}. Snapshot metadata.json is not affected.")
  }

  if (path_from_manifest) {
    if (!is.null(manifest)) {
      if (!manifest_file_col %in% colnames(manifest)) {
        cli::cli_abort("Passed {.code path = '<manifest>'}, but the manifest has no column {.field {manifest_file_col}}. Available manifest columns: [{colnames(manifest)}].")
      }

      if (any(is.na(manifest[[manifest_file_col]]) | manifest[[manifest_file_col]] == "")) {
        cli::cli_abort("Column {.field {manifest_file_col}} in manifest contains empty/NA paths. Please provide valid file paths for all rows.")
      }

      path <- normalizePath(manifest[[manifest_file_col]])
      manifest[[immundata_filename_col]] <- path
    } else {
      cli::cli_abort("Passed `<manifest>`, but no `manifest` table provided. Please provide either a list of file paths or a manifest.")
    }
  } else {
    path <- normalizePath(Sys.glob(path), mustWork = FALSE)

    if (!is.null(manifest) && immundata_filename_col %in% colnames(manifest)) {
      manifest[[immundata_filename_col]] <- normalizePath(
        manifest[[immundata_filename_col]],
        mustWork = FALSE
      )
    }
  }

  if (!is.null(manifest) && immundata_filename_col %in% colnames(manifest)) {
    assert_unique_manifest_paths(manifest[[immundata_filename_col]])
  }
  checkmate::assert_file_exists(path)

  resolved_repertoire_schema <- resolve_repertoire_schema(
    repertoire_schema = repertoire_schema,
    manifest = manifest,
    path_from_manifest = path_from_manifest,
    filename_col = immundata_filename_col
  )

  # Read the dataset
  if (verbose) {
    cli::cli_h3("Reading repertoire data")
  }
  file_check_results <- check_file_extensions(path, verbose = verbose)
  input_file_type <- file_check_results$filetype
  delim <- file_check_results$delim

  raw_dataset <- suppressMessages(switch(input_file_type,
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
  ))

  raw_dataset <- raw_dataset |>
    rename(!!immundata_filename_col := any_of("filename"))

  # Rename columns
  if (!is.null(rename_columns)) {
    if (verbose) {
      cli::cli_h3("Renaming the columns and schemas")
    }

    old_colnames <- colnames(raw_dataset)
    applied_rename_columns <- rename_columns[unname(rename_columns) %in% old_colnames]
    missing_rename_columns <- rename_columns[!unname(rename_columns) %in% old_colnames]

    raw_dataset <- raw_dataset |> rename(any_of(rename_columns))
    new_colnames <- colnames(raw_dataset)
    renamed_cols <- setdiff(new_colnames, old_colnames)
    if (length(renamed_cols) && verbose) {
      cli_alert_success("Introduced new renamed columns: {renamed_cols}")
    }

    for (i in seq_along(schema)) {
      if (schema[i] %in% rename_columns) {
        schema[i] <- names(rename_columns)[schema[i] == rename_columns]
      }
    }

    if (!is.null(resolved_repertoire_schema) && !repertoire_schema_was_special && is.character(resolved_repertoire_schema)) {
      for (i in seq_along(resolved_repertoire_schema)) {
        if (resolved_repertoire_schema[i] %in% rename_columns) {
          resolved_repertoire_schema[i] <- names(rename_columns)[resolved_repertoire_schema[i] == rename_columns]
        }
      }
    }

    if (verbose) {
      cli::cli_alert_success("Renaming is finished")
    }
  }

  #
  # Preprocess the data
  #
  if (length(preprocess)) {
    if (verbose) {
      cli::cli_h3("Preprocessing the data")
    }
    preprocess_input_cols <- colnames(raw_dataset)

    if (verbose) {
      ol <- cli::cli_ol()
      cli::cli_ol()
    }
    for (strategy_i in seq_along(preprocess)) {
      if (verbose) {
        cli::cli_li(names(preprocess)[strategy_i])
        raw_dataset <- preprocess[[strategy_i]](raw_dataset, manifest = manifest)
      } else {
        raw_dataset <- suppressMessages(preprocess[[strategy_i]](raw_dataset, manifest = manifest))
      }
    }
    if (verbose) {
      cli::cli_end()
      cli::cli_end(ol)
    }

    dropped_columns <- setdiff(preprocess_input_cols, colnames(raw_dataset))

    if (verbose) {
      cli::cli_alert_success("Preprocessing plan is ready")
    }
  }


  #
  # Aggregate the data
  #
  if (verbose) {
    cli::cli_h3("Aggregating the data to receptors")
  }

  if (verbose) {
    annotation_data <- agg_receptors(
      dataset = raw_dataset,
      schema = schema,
      barcode_col = barcode_col,
      count_col = count_col,
      locus_col = locus_col,
      umi_col = umi_col,
      verbose = verbose
    )
  } else {
    annotation_data <- suppressMessages(agg_receptors(
      dataset = raw_dataset,
      schema = schema,
      barcode_col = barcode_col,
      count_col = count_col,
      locus_col = locus_col,
      umi_col = umi_col,
      verbose = verbose
    ))
  }

  if (verbose) {
    cli::cli_alert_success("Execution plan for receptor data aggregation and annotation is ready")
  }

  #
  # Joining with the manifest table
  #
  if (!is.null(manifest)) {
    if (!immundata_filename_col %in% colnames(manifest)) {
      cli::cli_abort("No '{immundata_filename_col}' in the manifest. It is imperative to have this column - `immundata` uses it to annotate the AIRR files")
    }

    if (verbose) {
      cli::cli_h3("Joining the manifest with the dataset using '{immundata_filename_col}' column")
    }

    manifest_duckdb <- duckdb_tibble(manifest)

    annotation_data <- annotation_data |>
      left_join(manifest_duckdb, by = immundata_filename_col)

    if (verbose) {
      cli::cli_alert_success("Joining plan is ready")
    }
  }

  #
  # Postprocess the data
  #
  if (length(postprocess)) {
    if (verbose) {
      cli::cli_h3("Postprocessing the data")
    }

    if (verbose) {
      ol <- cli::cli_ol()
      cli::cli_ol()
    }
    for (strategy_i in seq_along(postprocess)) {
      if (verbose) {
        cli::cli_li(names(postprocess)[strategy_i])
        annotation_data <- postprocess[[strategy_i]](annotation_data)
      } else {
        annotation_data <- suppressMessages(postprocess[[strategy_i]](annotation_data))
      }
    }
    if (verbose) {
      cli::cli_end()
      cli::cli_end(ol)
    }

    if (verbose) {
      cli::cli_alert_success("Postprocessing plan is ready")
    }
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
  # Create repertoires
  #
  if (!is.null(resolved_repertoire_schema)) {
    if (verbose) {
      cli::cli_h3("Aggregating repertoires...")
    }
    if (verbose) {
      idata <- agg_repertoires(idata, resolved_repertoire_schema, verbose = verbose)
    } else {
      idata <- suppressMessages(
        agg_repertoires(idata, resolved_repertoire_schema, verbose = verbose)
      )
    }
    if (verbose) {
      cli_alert_success("Aggregation is finished")
    }
  }

  #
  # Save the created ImmunData on disk
  #
  if (verbose) {
    cli::cli_h3("Saving the newly created ImmunData to disk")
  }

  write_immundata_internal(
    idata = idata,
    output_folder = output_folder,
    producer_function = "read_repertoires",
    ingestion_payload = list(
      inputs = list(
        files = path,
        manifest_joined = !is.null(manifest),
        enforce_schema = enforce_schema
      ),
      args = list(
        barcode_col = barcode_col,
        count_col = count_col,
        locus_col = locus_col,
        umi_col = umi_col,
        manifest_file_col = manifest_file_col
      ),
      column_lineage = list(
        renamed = list(
          requested = requested_rename_columns,
          applied = applied_rename_columns,
          not_found = missing_rename_columns
        ),
        dropped = list(
          applied = dropped_columns
        )
      ),
      pipeline = list(
        preprocess = names(preprocess),
        postprocess = names(postprocess)
      )
    ),
    verbose = verbose
  )

  #
  # ... and load it again so the source will be fast Parquet files
  #
  idata <- read_immundata(output_folder, verbose = FALSE)

  if (verbose) {
    cli::cli_h3("Summary")
  }
  final_time <- format(round(Sys.time() - start_time, 2))
  if (verbose) {
    cli_alert_info("Time elapsed: {.emph {final_time}}")
  }

  idata_size <- idata |>
    count() |>
    pull("n")

  idata_receptors <- idata$annotations |>
    distinct(!!imd_schema_sym("receptor")) |>
    count() |>
    pull("n")

  if (verbose) {
    cli_alert_success("Loaded ImmunData with the receptor schema: [{schema}]")
  }

  if (!is.null(resolved_repertoire_schema) && verbose) {
    cli_alert_success("Loaded ImmunData with the repertoire schema: [{resolved_repertoire_schema}]")
  }

  if (verbose) {
    if (idata_size == 0) {
      cli_alert_warning("Loaded ImmunData with zero (!) chains. Possible problems: wrong {.code 'chain'} specification to the receptor schema (e.g., {.code 'TCRB'} instead of {.code 'TRB'}), or preproces/postprocess filters")
    } else {
      cli_alert_success("Loaded ImmunData with [{idata_size}] chains and [{idata_receptors}] receptors")
    }
  }

  idata
}

is_special_repertoire_schema <- function(repertoire_schema, value = NULL) {
  is_special <- is.character(repertoire_schema) &&
    length(repertoire_schema) == 1 &&
    repertoire_schema %in% c("<auto>", "<manifest>")

  if (is.null(value)) {
    return(is_special)
  }

  is_special && identical(repertoire_schema, value)
}

assert_unique_manifest_paths <- function(paths) {
  duplicated_paths <- unique(paths[duplicated(paths)])

  if (length(duplicated_paths) == 0) {
    return(invisible(TRUE))
  }

  duplicated_path_details <- vapply(
    duplicated_paths,
    function(duplicated_path) {
      duplicated_rows <- which(paths == duplicated_path)
      paste0(
        duplicated_path,
        " (rows ",
        paste(duplicated_rows, collapse = ", "),
        ")"
      )
    },
    character(1)
  )

  cli::cli_abort(c(
    "Manifest contains duplicated repertoire file paths after normalization.",
    "!" = "Each repertoire file must appear only once.",
    "x" = "Duplicated paths: {paste(duplicated_path_details, collapse = '; ')}"
  ))
}

resolve_repertoire_schema <- function(repertoire_schema,
                                      manifest,
                                      path_from_manifest,
                                      filename_col) {
  if (is.null(repertoire_schema) || is.function(repertoire_schema)) {
    return(repertoire_schema)
  }

  if (is_special_repertoire_schema(repertoire_schema, "<auto>")) {
    if (isTRUE(path_from_manifest)) {
      repertoire_schema <- "<manifest>"
    } else {
      return(filename_col)
    }
  }

  if (is_special_repertoire_schema(repertoire_schema, "<manifest>")) {
    if (!is.null(manifest)) {
      return(colnames(manifest))
    }

    return(filename_col)
  }

  repertoire_schema
}

check_file_extensions <- function(path, verbose = TRUE) {
  if (verbose) {
    ol <- cli_ol()
    cli_ol(path)
    cli_end(ol)

    cli_alert_info("Checking if all files are of the same type")
  }

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
    if (verbose) {
      cli_alert_success("All files have the same extension")
    }
  } else {
    cli_abort("Not all files of the same type. Please convert them all to the same type, and try again")
  }

  list(filetype = input_file_type, delim = delim)
}
