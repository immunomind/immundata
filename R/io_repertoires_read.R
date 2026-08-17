#' @title Read immune repertoire files into ImmunData
#'
#' @description
#' `read_repertoires()` is the main function for importing AIRR-seq data. It
#' reads one or more repertoire files, defines biological receptors, adds
#' sample information from an optional manifest, and returns an [ImmunData]
#' object.
#'
#' The function saves the processed data in `output_folder`. This lets you work
#' with large datasets without loading everything into memory and reopen the
#' result later with [read_immundata()].
#'
#' @param path One or more repertoire file paths, or a glob pattern such as
#'   `"/path/to/data/*.tsv.gz"`. Supported formats are Parquet, CSV, TSV, and
#'   gzipped CSV or TSV. All input files must have the same file type.
#'
#'   Use `"<manifest>"` to take file paths from `manifest` instead. In that
#'   case, `manifest` is required.
#' @param schema Definition of receptor identity. Supply either:
#'
#'   * A character vector naming the features that must match, such as
#'     `c("v_call", "j_call", "junction_aa")`.
#'   * An object created by [make_receptor_schema()] to select one locus or pair
#'     two loci from the same cell.
#'
#'   Use column names as they appear *after* `rename_columns` is applied. For
#'   example, if the input columns are `CDR3.aa` and `V.name`, use
#'   `rename_columns = c(cdr3_aa = "CDR3.aa", v_call = "V.name")` together with
#'   `schema = c("cdr3_aa", "v_call")`.
#' @param manifest An optional data frame with one row per repertoire file and
#'   columns containing sample, donor, tissue, treatment, or other information.
#'   Use [read_manifest()] to read and validate a manifest file. Manifest paths
#'   must be unique. When `path = "<manifest>"`, the column named by
#'   `manifest_file_col` supplies the repertoire file paths. The default is
#'   `NULL`.
#' @param barcode_col Name of the column containing cell barcodes. Supplying it
#'   selects single-cell processing, requires `umi_col`, and prevents use of
#'   `count_col`. Use the column name after renaming. The default is `NULL`.
#' @param count_col Name of the column containing non-negative abundance values
#'   for bulk repertoire data. It cannot be used with `barcode_col`. Use the
#'   column name after renaming. The default is `NULL`.
#' @param locus_col Name of the column containing receptor loci such as `"TRA"`,
#'   `"TRB"`, `"IGH"`, `"IGK"`, or `"IGL"`. It is required when `schema`
#'   selects or pairs chains. Use the column name after renaming. The default is
#'   `NULL`.
#' @param umi_col Name of the column containing per-chain UMI or read counts.
#'   It is required whenever `barcode_col` is supplied and is used to choose one
#'   chain when a cell contains several chains from the same locus. Use the
#'   column name after renaming. The default is `NULL`.
#' @param preprocess A named list of functions applied in order before receptors
#'   are defined. Each function must accept a duckplyr table as its first
#'   argument and return a duckplyr table. By default,
#'   [make_default_preprocessing()] removes selected technical columns and keeps
#'   productive sequences when a `productive` column is available. Use `NULL`
#'   or `list()` to disable preprocessing.
#' @param postprocess A named list of functions applied in order after receptors
#'   are defined and manifest information is added. Each function must accept
#'   and return a duckplyr table. By default, [make_default_postprocessing()]
#'   prefixes cell barcodes when the manifest contains a `Prefix` column. Use
#'   `NULL` or `list()` to disable postprocessing.
#' @param rename_columns An optional named character vector in the form
#'   `c(new_name = "old_name")`. Renaming occurs before preprocessing and
#'   receptor definition. The default, `imd_rename_cols("10x")`, standardizes
#'   common 10x names such as `v_gene` to `v_call` and `chain` to `locus` when
#'   those source columns are present. Use `NULL` to preserve all input names.
#' @param enforce_schema Whether multiple input files must have the same columns
#'   and column types. The default is `TRUE`. If `FALSE`, columns are combined
#'   by name and missing values are added where necessary. This is slower and
#'   can require more memory.
#' @param manifest_file_col Name of the manifest column containing repertoire
#'   file paths when `path = "<manifest>"`. The default is `"file"`. Use the
#'   same name passed as `file_col` to [read_manifest()] when it is not `"file"`.
#' @param output_folder Directory in which to write `annotations.parquet` and
#'   `metadata.json`. These files are the persistent backing storage for the
#'   returned object. If `NULL`, a folder beginning with `immundata-` is created
#'   beside the first input file. Supplying an existing folder replaces its
#'   `annotations.parquet` and `metadata.json`. The default is `NULL`.
#' @param repertoire_schema Definition of repertoires. Supply one of:
#'
#'   * A character vector naming columns that define one repertoire, such as
#'     `c("donor", "timepoint")`.
#'   * `"<auto>"`, the default. This creates one repertoire per input file, or
#'     one per manifest row when `path = "<manifest>"`.
#'   * `"<manifest>"`, which uses all manifest columns when a manifest is
#'     available, or the input filename otherwise.
#'   * `NULL` to leave repertoires undefined.
#' @param verbose Whether to print progress and summary messages. Defaults to
#'   `getOption("immundata.verbose", TRUE)`.
#'
#' @details
#' The required arguments depend on how receptor observations are represented in
#' the input files.
#'
#' @section Choose arguments for your data:
#'
#' * **Uncounted repertoire table:** Supply `schema`. Leave `barcode_col` and
#'   `count_col` as `NULL`. Each retained row represents one observed chain.
#' * **Bulk repertoire with abundance:** Supply `schema` and `count_col`. The
#'   abundance values are preserved for later repertoire statistics.
#' * **Single-cell, one selected chain:** Use [make_receptor_schema()] with one
#'   chain and supply `barcode_col`, `locus_col`, and `umi_col`.
#' * **Single-cell, paired chains:** Use [make_receptor_schema()] with two chains
#'   and supply `barcode_col`, `locus_col`, and `umi_col`. Only cells containing
#'   both requested chains are retained.
#' * **Single-cell, relaxed paired chains:** Use a schema such as
#'   `chains = c("IGH", "IGL|IGK")` with `barcode_col`, `locus_col`, and
#'   `umi_col`. This accepts either an IGH-IGL or IGH-IGK receptor.
#'
#' In single-cell data, the chain with the highest `umi_col` value is retained
#' when a cell contains several chains from the same locus.
#'
#' @section What happens by default:
#'
#' Unless you override the relevant arguments, `read_repertoires()`:
#'
#' * standardizes common 10x column names;
#' * removes selected technical columns;
#' * keeps productive sequences when productivity information is present;
#' * prefixes barcodes when a manifest `Prefix` column is present;
#' * creates repertoires automatically; and
#' * writes the completed dataset to disk.
#'
#' Set `rename_columns`, `preprocess`, `postprocess`, or `repertoire_schema` to
#' `NULL` to disable the corresponding behavior.
#'
#' @section Processing order:
#'
#' The function:
#'
#' 1. finds and reads the input files as one duckplyr table;
#' 2. renames columns;
#' 3. applies preprocessing;
#' 4. defines receptors using `schema`;
#' 5. adds manifest information;
#' 6. applies postprocessing;
#' 7. defines repertoires when requested; and
#' 8. writes and reopens the completed [ImmunData] dataset.
#'
#' @section Manifests and repertoires:
#'
#' A manifest *annotates* each input file with biological information. The
#' `repertoire_schema` argument chooses which annotation columns *define a
#' repertoire* and therefore determine receptor counts and proportions.
#'
#' With `path = "<manifest>"` and the default `repertoire_schema = "<auto>"`,
#' all manifest columns are used and each manifest row becomes one repertoire.
#' With an explicit file path or vector of paths, `"<auto>"` creates one
#' repertoire per input file.
#'
#' @section Output storage:
#'
#' The output folder is not a temporary cache. The returned object reads its
#' receptor annotations from `annotations.parquet`, while `metadata.json` stores
#' its schemas, repertoire summaries, and provenance. Keep this folder for as
#' long as you need the object, or reopen it later with [read_immundata()].
#'
#' **Important:** Reusing the same `output_folder` replaces the existing
#' `annotations.parquet` and `metadata.json` without creating a new version.
#'
#' @return A disk-backed [ImmunData] object containing the retained chain rows,
#'   receptor definitions, manifest annotations, and ingestion provenance. If
#'   `repertoire_schema` is not `NULL`, it also contains repertoire definitions
#'   and summary statistics calculated by [agg_repertoires()].
#'
#' @seealso [read_manifest()], [make_receptor_schema()], [agg_receptors()],
#'   [agg_repertoires()], [make_default_preprocessing()],
#'   [make_default_postprocessing()], [read_immundata()], [write_immundata()],
#'   [ImmunData]
#'
#' @concept ingestion
#' @export
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Read one bulk AIRR file and preserve its abundance column
#' bulk_file <- system.file(
#'   "extdata/tsv",
#'   "sample_0_1k.tsv",
#'   package = "immundata"
#' )
#'
#' bulk_idata <- read_repertoires(
#'   path = bulk_file,
#'   schema = c("cdr3_aa", "v_call"),
#'   count_col = "counts",
#'   output_folder = tempfile("immundata-bulk-")
#' )
#'
#' tibble(
#'   n_records = bulk_idata |> count() |> pull(n),
#'   n_receptors = bulk_idata$receptors |> count() |> collect() |> pull(n),
#'   n_repertoires = nrow(bulk_idata$repertoires)
#' )
#' # Expected result:
#' #   n_records n_receptors n_repertoires
#' #         955         871             1
#'
#' # Read multiple files and their sample information from a manifest
#' manifest_path <- system.file(
#'   "extdata/tsv",
#'   "manifest.csv",
#'   package = "immundata"
#' )
#' manifest <- read_manifest(manifest_path)
#'
#' manifest_idata <- read_repertoires(
#'   path = "<manifest>",
#'   manifest = manifest,
#'   schema = c("cdr3_aa", "v_call"),
#'   count_col = "counts",
#'   output_folder = tempfile("immundata-manifest-")
#' )
#'
#' manifest_idata$repertoires |>
#'   select(Therapy, Response, n_barcodes, n_receptors) |>
#'   arrange(Response)
#' # Expected result:
#' #   Therapy Response n_barcodes n_receptors
#' #   ICI     FR             4725         871
#' #   CAR-T   PR             4758         867
#'
#' # Read paired TRA-TRB receptors from a small single-cell table
#' paired_input <- tibble(
#'   cell_id = c("cell1", "cell1", "cell2", "cell2", "cell3"),
#'   locus = c("TRA", "TRB", "TRA", "TRB", "TRA"),
#'   v_call = c("TRAV1", "TRBV1", "TRAV1", "TRBV1", "TRAV2"),
#'   j_call = c("TRAJ1", "TRBJ1", "TRAJ1", "TRBJ1", "TRAJ2"),
#'   junction_aa = c("CAVA", "CASSB", "CAVA", "CASSB", "CAVC"),
#'   umi_count = c(10L, 8L, 12L, 9L, 7L)
#' )
#' paired_file <- tempfile(fileext = ".tsv")
#' readr::write_tsv(paired_input, paired_file)
#'
#' paired_idata <- read_repertoires(
#'   path = paired_file,
#'   schema = make_receptor_schema(
#'     features = c("v_call", "j_call", "junction_aa"),
#'     chains = c("TRA", "TRB")
#'   ),
#'   barcode_col = "cell_id",
#'   locus_col = "locus",
#'   umi_col = "umi_count",
#'   repertoire_schema = NULL,
#'   output_folder = tempfile("immundata-paired-")
#' )
#'
#' tibble(
#'   n_chains = paired_idata |> count() |> pull(n),
#'   n_cells = paired_idata |> collect() |> distinct(imd_barcode) |> nrow(),
#'   n_receptors = paired_idata$receptors |> count() |> collect() |> pull(n)
#' )
#' # Expected result:
#' #   n_chains n_cells n_receptors
#' #          4       2           1
#'
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
