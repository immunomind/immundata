# TODO: Use rlang::.data
utils::globalVariables(c("dd", "meta", "n_cells", "n_barcodes", "p", "tmp_receptor_cells", "temp__reads"))

# TODO: move globals to config file to share between R and Python

#' @title Internal Immundata Global Configuration
#'
#' @description
#' `IMD_GLOBALS` is an internal list that stores globally used constants across the Immundata system.
#' It is not intended for direct use by package users, but rather to ensure consistency in schema
#' field names, default file names, and internal error messages.
#'
#' @section Components:
#' - `messages`: Named list of default messages and error texts (e.g., `"NotImpl"`).
#' - `schema`: Standardized column names for internal schema usage. These include:
#'     - `cell`: Column name for cell barcode IDs.
#'     - `receptor`: Column name for receptor unique identifiers.
#'     - `repertoire`: Column name for repertoire group IDs.
#'     - `manifest_filename`: Column name for manifest file paths (internal).
#'     - `count`: Column name for receptor count per group.
#' - `files`: Default file names used to store structured Immundata:
#'     - `metadata`: File name for schemas and small summary tables (`metadata.json`).
#'     - `annotations`: File name for chain-level data (`annotations.parquet`).
#'
#' @keywords internal
IMD_GLOBALS <- list(
  schema = list(
    receptor = "imd_receptor_id",
    cell = "imd_barcode",
    barcode = "imd_barcode",
    chain = "imd_chain_id",
    group = "imd_group_id",
    repertoire = "imd_repertoire_id",
    strata = "imd_strata_id",
    strata_name = "strata_name",
    manifest_filename = "imd_filename",
    count = "imd_count",
    receptor_count = "imd_count",
    chain_count = "imd_n_chains",
    proportion = "imd_proportion",
    n_receptors = "n_receptors",
    n_barcodes = "n_barcodes",
    n_cells = "n_cells",
    n_repertoires = "n_repertoires",
    locus = "locus",
    sim_exact = "imd_sim_exact_",
    sim_regex = "imd_sim_regex_",
    sim_hamm = "imd_sim_hamm_",
    sim_lev = "imd_sim_lev_"
  ),
  meta_schema = list(
    format_version = "format_version",
    package_version = "package_version",
    schema_receptor = "schema_receptor",
    schema_repertoire = "schema_repertoire",
    schema_strata = "schema_strata",
    repertoires = "repertoires",
    producer = "producer",
    snapshot_id = "snapshot_id",
    lineage = "lineage",
    provenance = "provenance",
    extensions = "extensions"
  ),
  files = list(
    metadata = "metadata.json",
    annotations = "annotations.parquet"
  ),
  rename_cols = list(
    default = c(
      v_call = "v_gene",
      d_call = "d_gene",
      j_call = "j_gene",
      d_call = "d_gene",
      locus = "chain"
    ),
    `10x` =
      c(
        v_call = "v_gene",
        d_call = "d_gene",
        j_call = "j_gene",
        d_call = "d_gene",
        locus = "chain"
      )
  ),
  drop_cols = list(
    universal = c(
      "v_score", "d_score", "j_score", "stop_codon", "vj_in_frame",
      "v_sequence_alignment", "d_sequence_alignment", "j_sequence_alignment",
      "rev_comp", "sequence_alignment", "germline_alignment",
      "v_cigar", "d_cigar", "j_cigar", "full_length",
      "is_cell",
      "contig_id",
      "raw_clonotype_id",
      "raw_consensus_id",
      "high_confidence",
      "length"
    ),
    airr = c(
      "v_score", "d_score", "j_score", "stop_codon", "vj_in_frame",
      "v_sequence_alignment", "d_sequence_alignment", "j_sequence_alignment",
      "rev_comp", "sequence_alignment", "germline_alignment",
      "v_cigar", "d_cigar", "j_cigar"
    ),
    `10x` = c(
      "full_length",
      "is_cell",
      "contig_id",
      "raw_clonotype_id",
      "raw_consensus_id",
      "high_confidence",
      "length"
    )
  ),
  agg_schema = list(
    receptors = list(
      airr = list(
        locus = list(
          col_name = "locus",
          locus_pairs = list(
            c("TRA", "TRB"),
            c("TRG", "TRD"),
            c("IGH", "IGK"),
            c("IGH", "IGL")
          )
        )
      )
    )
  )
)

#' @title Get a standard ImmunData column name
#'
#' @description
#' Use `imd_schema()` when code needs the standard column name for an
#' `ImmunData` identifier or calculated value, such as the cell barcode,
#' receptor identifier, repertoire identifier, count, or proportion.
#'
#' Use this helper in reusable analysis code or package extensions instead of
#' writing an internal name such as `"imd_barcode"` directly. It only returns
#' names; it does not inspect or change an [ImmunData] object.
#'
#' @param key A character string or `NULL`. One schema key, for example
#'   `"barcode"`, `"receptor"`, `"repertoire"`, `"count"`, or
#'   `"proportion"`. Use `NULL`, the default, to return all available keys and
#'   column names.
#'
#' @return If `key` is supplied, one character string containing the standard
#'   column name. If `key = NULL`, a named list of all schema keys and column
#'   names.
#'
#' @seealso [make_receptor_schema()], [imd_rename_cols()], [ImmunData]
#'
#' @examples
#' imd_schema("barcode")
#' # Expected result: "imd_barcode"
#'
#' imd_schema("receptor")
#' # Expected result: "imd_receptor_id"
#'
#' # Use a returned name for programmatic selection.
#' barcode_column <- imd_schema("barcode")
#' get_test_idata() |>
#'   dplyr::collect() |>
#'   dplyr::select(dplyr::all_of(barcode_column)) |>
#'   head(2)
#'
#' @concept schema
#' @export
imd_schema <- function(key = NULL) {
  if (is.null(key)) {
    IMD_GLOBALS$schema
  } else {
    checkmate::assert_choice(key, names(IMD_GLOBALS$schema))
    IMD_GLOBALS$schema[[key]]
  }
}

#' @title Developer helpers for ImmunData schemas and storage
#'
#' @description
#' These helpers expose package constants for extension developers. They are not
#' needed for routine biological analysis. Use [imd_schema()] for a standard
#' column name and [make_receptor_schema()] to define biological receptors.
#'
#' The functions remain exported for compatibility with packages that extend
#' `immundata`, but their values describe implementation details and may grow as
#' the storage format develops.
#'
#' @param key A character string or `NULL`. For `imd_schema_sym()`, one schema
#'   key accepted by [imd_schema()]. Use `NULL`, the default, to return the
#'   complete named schema list.
#' @param format A character string. For `imd_repertoire_schema()`, the preset
#'   name. Currently only `"airr"` is accepted.
#' @param schema A receptor-schema list. For `imd_receptor_features()` and
#'   `imd_receptor_chains()`, a schema created by [make_receptor_schema()].
#'
#' @return
#' * `imd_schema_sym()` returns an rlang symbol for one standard column. With
#'   `key = NULL`, it returns the complete named schema list.
#' * `imd_meta_schema()` returns a named list of fields used in
#'   `metadata.json`.
#' * `imd_files()` returns a named list of standard snapshot file names.
#' * `imd_repertoire_schema()` returns the configured preset for `format`, or
#'   `NULL` when no preset is configured.
#' * `imd_receptor_features()` returns the character vector in
#'   `schema$features`.
#' * `imd_receptor_chains()` returns the character vector in `schema$chains`, or
#'   `NULL` for a chain-agnostic schema.
#'
#' @examples
#' schema <- make_receptor_schema(
#'   features = c("junction_aa", "v_call"),
#'   chains = c("TRA", "TRB")
#' )
#'
#' imd_receptor_features(schema)
#' # Expected result: c("junction_aa", "v_call")
#'
#' imd_receptor_chains(schema)
#' # Expected result: c("TRA", "TRB")
#'
#' imd_files()
#' # Lists the standard metadata and Parquet file names.
#'
#' @keywords internal
#' @concept schema
#' @export
imd_schema_sym <- function(key = NULL) {
  if (is.null(key)) {
    IMD_GLOBALS$schema
  } else {
    checkmate::assert_choice(key, names(IMD_GLOBALS$schema))
    rlang::sym(IMD_GLOBALS$schema[[key]])
  }
}

#' @rdname imd_schema_sym
#' @export
imd_meta_schema <- function() {
  # TODO: pass value to the function
  IMD_GLOBALS$meta_schema
}

#' @rdname imd_schema_sym
#' @export
imd_files <- function() {
  IMD_GLOBALS$files
}

#' @title Get input-column presets
#'
#' @description
#' Use these helpers to inspect or customize the column renaming and removal
#' presets used by [read_repertoires()].
#'
#' `imd_rename_cols()` returns mappings from standard output names to source
#' names. `imd_drop_cols()` returns technical columns that can usually be
#' removed before receptors are defined. These functions return definitions
#' only; they do not change input files or an [ImmunData] object.
#'
#' @param format A character string. The input format preset. For
#'   `imd_rename_cols()`, use `"default"` or `"10x"`; the default is
#'   `"default"`. For `imd_drop_cols()`, use `"universal"`, `"airr"`, or
#'   `"10x"`; the default is `"airr"`.
#'
#' @return `imd_rename_cols()` returns a named character vector in the form
#'   `c(new_name = "source_name")`. `imd_drop_cols()` returns a character
#'   vector of source columns to remove.
#'
#' @seealso [read_repertoires()], [make_default_preprocessing()], [imd_schema()]
#'
#' @examples
#' imd_rename_cols("10x")
#' # Includes c(v_call = "v_gene", locus = "chain").
#'
#' head(imd_drop_cols("10x"), 3)
#' # Expected result:
#' #   "full_length" "is_cell" "contig_id"
#'
#' # Keep the 10x `contig_id` column while dropping the other default columns.
#' columns_to_drop <- setdiff(imd_drop_cols("10x"), "contig_id")
#' custom_preprocessing <- list(
#'   exclude_columns = make_exclude_columns(columns_to_drop),
#'   filter_nonproductive = make_productive_filter(
#'     truthy = c("TRUE", "true", "1")
#'   )
#' )
#'
#' @concept ingestion
#' @rdname imd_input_columns
#' @export
imd_rename_cols <- function(format = "default") {
  checkmate::assert_character(format)
  checkmate::assert_choice(format, names(IMD_GLOBALS$rename_cols))

  IMD_GLOBALS$rename_cols[[format]]
}

#' @rdname imd_input_columns
#' @export
imd_drop_cols <- function(format = "airr") {
  checkmate::assert_character(format)
  checkmate::assert_choice(format, names(IMD_GLOBALS$drop_cols))

  IMD_GLOBALS$drop_cols[[format]]
}

#' @rdname imd_schema_sym
#' @export
imd_repertoire_schema <- function(format = "airr") {
  checkmate::assert_character(format)
  checkmate::assert_choice(format, c("airr"))

  IMD_GLOBALS$agg_schema$repertoires[[format]]
}

#' @rdname imd_schema_sym
#' @export
imd_receptor_features <- function(schema) {
  schema[["features"]]
}

#' @rdname imd_schema_sym
#' @export
imd_receptor_chains <- function(schema) {
  schema[["chains"]]
}
