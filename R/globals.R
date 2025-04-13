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
    cell = "imd_cell_id",
    receptor = "imd_receptor_id",
    repertoire = "imd_repertoire_id",
    metadata_filename = "imd_filename",
    count = "imd_count",
    proportion = "imd_proportion",
    n_receptors = "n_receptors",
    n_cells = "n_cells",
    filename = "filename"
  ),
  files = list(
    receptors = "receptors.parquet",
    annotations = "annotations.parquet"
  ),
  rename_cols = list(
    default = function() {
      c(
        v_call = any_of(c("v_gene")),
        d_call = any_of(c("d_gene")),
        j_call = any_of(c("j_gene")),
        d_call = any_of(c("d_gene")),
        locus = any_of(c("chain"))
      )
    },
    `10x` = function() {
      c(
        v_call = any_of(c("v_gene")),
        d_call = any_of(c("d_gene")),
        j_call = any_of(c("j_gene")),
        d_call = any_of(c("d_gene")),
        locus = any_of(c("chain"))
      )
    }
  ),
  drop_cols = list(
    airr = c(
      "v_score", "d_score", "j_score", "stop_codon", "vj_in_frame",
      "v_sequence_alignment", "d_sequence_alignment", "j_sequence_alignment",
      "rev_comp", "sequence_alignment", "germline_alignment",
      "v_cigar", "d_cigar", "j_cigar"
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
    ),
    repertoires = list(
      airr = function() {
        any_of(
          filename = "filename",
          sample = "sample",
          repertoire_id = "repertoire_id"
        )
      }
    )
  )
)

#' @title Get Immundata Internal Schema Field Names
#'
#' @description
#' Returns the standardized field names used across Immundata objects and processing functions,
#' as defined in `IMD_GLOBALS$schema`. These include column names for cell ids or barcodes, receptors,
#' repertoires, and related metadata.
#'
#' @return A named list of schema field names.
#' @export
imd_schema <- function(val = NULL) {
  # TODO: pass value to the function
  IMD_GLOBALS$schema
}

#' @title Get Immundata Default File Names
#'
#' @description
#' Returns the standardized default filenames for storing receptor-level and annotation-level
#' data as used in `read_repertoires()` and related Immundata I/O functions.
#'
#' @return A named list of file names (e.g., `receptors.parquet`, `annotations.parquet`).
#' @export
imd_files <- function() {
  IMD_GLOBALS$files
}

#' @export
imd_rename_cols <- function(format = "default") {
  check_character(format)
  check_choice(format, names(IMD_GLOBALS$rename_cols))

  IMD_GLOBALS$rename_cols[[format]]
}

#' @export
imd_drop_cols <- function(format = "airr") {
  check_character(format)
  check_choice(format, names(IMD_GLOBALS$drop_cols))

  IMD_GLOBALS$drop_cols[[format]]
}

#' @export
imd_repertoire_schema <- function(format = "airr") {
  check_character(format)
  check_choice(format, c("airr"))

  IMD_GLOBALS$agg_schema$repertoires[[format]]
}
