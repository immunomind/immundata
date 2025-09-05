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
#'     - `metadata_filename`: Column name for metadata files (internal).
#'     - `count`: Column name for receptor count per group.
#'     - `filename`: Original column name used in user metadata.
#' - `files`: Default file names used to store structured Immundata:
#'     - `receptors`: File name for receptor-level data (`receptors.parquet`).
#'     - `annotations`: File name for annotation-level data (`annotations.parquet`).
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
    metadata_filename = "imd_filename",
    count = "imd_count",
    receptor_count = "imd_count",
    chain_count = "imd_n_chains",
    proportion = "imd_proportion",
    n_receptors = "n_receptors",
    n_barcodes = "n_barcodes",
    n_cells = "n_cells",
    n_repertoires = "n_repertoires",
    filename = "filename",
    locus = "locus",
    sim_exact = "imd_sim_exact_",
    sim_regex = "imd_sim_regex_",
    sim_hamm = "imd_sim_hamm_",
    sim_lev = "imd_sim_lev_"
  ),
  meta_schema = list(
    version = "version",
    receptor_schema = "receptor_schema",
    repertoire_schema = "repertoire_schema"
  ),
  files = list(
    metadata = "metadata.json",
    receptors = "receptors.parquet",
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

#' @title Get Immundata internal schema field names
#'
#' @description
#' Returns the standardized field names used across Immundata objects and processing functions,
#' as defined in `IMD_GLOBALS$schema`. These include column names for cell ids or barcodes, receptors,
#' repertoires, and related metadata.
#'
#' @param key Character which field to return.
#' @param format Character what format to load - "airr" or "10x".
#' @param schema Receptor schema from [make_receptor_schema()].
#'
#' @concept schema
#'
#' @export
imd_schema <- function(key = NULL) {
  if (is.null(key)) {
    IMD_GLOBALS$schema
  } else {
    checkmate::assert_choice(key, names(IMD_GLOBALS$schema))
    IMD_GLOBALS$schema[[key]]
  }
}

#' @rdname imd_schema
#' @export
imd_schema_sym <- function(key = NULL) {
  if (is.null(key)) {
    IMD_GLOBALS$schema
  } else {
    checkmate::assert_choice(key, names(IMD_GLOBALS$schema))
    rlang::sym(IMD_GLOBALS$schema[[key]])
  }
}

#' @rdname imd_schema
#' @export
imd_meta_schema <- function() {
  # TODO: pass value to the function
  IMD_GLOBALS$meta_schema
}

#' @rdname imd_schema
#' @export
imd_files <- function() {
  IMD_GLOBALS$files
}

#' @rdname imd_schema
#' @export
imd_rename_cols <- function(format = "default") {
  checkmate::assert_character(format)
  checkmate::assert_choice(format, names(IMD_GLOBALS$rename_cols))

  IMD_GLOBALS$rename_cols[[format]]
}

#' @rdname imd_schema
#' @export
imd_drop_cols <- function(format = "airr") {
  checkmate::assert_character(format)
  checkmate::assert_choice(format, names(IMD_GLOBALS$drop_cols))

  IMD_GLOBALS$drop_cols[[format]]
}

#' @rdname imd_schema
#' @export
imd_repertoire_schema <- function(format = "airr") {
  checkmate::assert_character(format)
  checkmate::assert_choice(format, c("airr"))

  IMD_GLOBALS$agg_schema$repertoires[[format]]
}

#' @rdname imd_schema
#' @export
imd_receptor_features <- function(schema) {
  schema[["features"]]
}

#' @rdname imd_schema
#' @export
imd_receptor_chains <- function(schema) {
  schema[["chains"]]
}
