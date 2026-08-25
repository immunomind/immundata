#' @title Define which chain observations form the same receptor
#'
#' @description
#' Use `make_receptor_schema()` to define a biological receptor from sequence
#' features and one or two receptor chains.
#'
#' Use this function when reading single-cell data with one selected chain, when
#' pairing chains such as TRA-TRB, or when accepting alternative light chains
#' such as IGK or IGL. The unit being defined is the receptor. Creating a schema
#' does not change any data or an existing [ImmunData] object.
#'
#' @section When observations are the same receptor:
#'
#' `features` names the fields that define chain identity. Common choices are
#' the CDR3 amino acid sequence, V gene, and J gene. Two observations represent
#' the same receptor only when the relevant chain loci and every selected
#' feature match.
#'
#' * With one chain, such as `chains = "TRB"`, only that locus is used. Two TRB
#'   observations are the same receptor when all their selected feature values
#'   match.
#' * With a strict pair, such as `chains = c("TRA", "TRB")`, chains are first
#'   paired within each cell barcode. Receptors from two cells are the same only
#'   when every selected TRA feature and every selected TRB feature match.
#' * With an alternative second chain, such as
#'   `chains = c("IGH", "IGK|IGL")`, each receptor must contain IGH and exactly
#'   one of IGK or IGL. Cells containing both IGK and IGL are excluded. The
#'   light-chain locus and all selected heavy- and light-chain features must
#'   match for two observations to be the same receptor.
#'
#' A barcode determines which chains belong to the same cell; it does not by
#' itself define receptor identity across cells. During single-cell import,
#' [read_repertoires()] uses `umi_col` to choose one chain when a cell contains
#' several observations from the same locus.
#'
#' Use `chains = NULL` for chain-agnostic bulk or pre-filtered data. In that
#' case, only the values in `features` define receptor identity.
#'
#' @section Validate a schema:
#'
#' `assert_receptor_schema()` stops with an error if `schema` is not accepted.
#' Use it inside another function when invalid input must stop the calculation.
#' `test_receptor_schema()` returns one `TRUE` or `FALSE` value and is useful in
#' conditional code.
#'
#' @section Backend and storage:
#'
#' A receptor schema is a small R list containing `features` and `chains`. It
#' stores no sequence data. [read_repertoires()] and [agg_receptors()] apply the
#' schema to chain observations using duckplyr.
#'
#' @param features A non-empty character vector. Column names containing the
#'   chain fields that must match, such as
#'   `c("junction_aa", "v_call", "j_call")`. Use names as they appear after any
#'   input-column renaming.
#' @param chains A character vector of length one or two, or `NULL`. Use one
#'   value, such as `"TRB"`, to keep one chain; two values, such as
#'   `c("TRA", "TRB")`, to define a strict pair; or the `"IGK|IGL"` syntax in
#'   the second value to accept either alternative. The default is `NULL`, which
#'   does not select loci.
#' @param schema A non-empty character vector or receptor-schema list. An object
#'   to check. A schema created by `make_receptor_schema()` is accepted. A
#'   character vector supplies feature names for a chain-agnostic schema.
#'
#' @return `make_receptor_schema()` returns a list with character elements
#'   `features` and `chains`; `chains` is `NULL` when loci are not selected.
#'   `assert_receptor_schema()` returns `TRUE` for accepted input and otherwise
#'   stops with an error. `test_receptor_schema()` returns one logical value.
#'
#' @seealso [read_repertoires()], [agg_receptors()], [imd_schema()]
#'
#' @examples
#' # Single-chain TCR: compare TRB observations by CDR3, V gene, and J gene.
#' trb_schema <- make_receptor_schema(
#'   features = c("junction_aa", "v_call", "j_call"),
#'   chains = "TRB"
#' )
#' trb_schema
#' # Expected result:
#' #   $features: "junction_aa" "v_call" "j_call"
#' #   $chains:   "TRB"
#'
#' # Paired alpha-beta TCR: all selected fields must match on both TRA and TRB.
#' ab_tcr_schema <- make_receptor_schema(
#'   features = c("junction_aa", "v_call", "j_call"),
#'   chains = c("TRA", "TRB")
#' )
#' ab_tcr_schema
#' # The result defines one receptor as a matched TRA-TRB pair from one cell.
#'
#' # BCR: require IGH and accept either an IGK or IGL light chain.
#' bcr_schema <- make_receptor_schema(
#'   features = c("junction_aa", "v_call", "j_call"),
#'   chains = c("IGH", "IGK|IGL")
#' )
#' bcr_schema
#' # The result accepts IGH-IGK and IGH-IGL receptors, while keeping the two
#' # light-chain loci biologically distinct.
#'
#' test_receptor_schema(bcr_schema)
#' # Expected result: TRUE
#'
#' @rdname make_receptor_schema
#' @concept utils
#' @export
make_receptor_schema <- function(features, chains = NULL) {
  checkmate::assert_character(features, min.len = 1, any.missing = FALSE)
  checkmate::assert_character(
    chains,
    min.len = 1,
    max.len = 2,
    any.missing = FALSE,
    null.ok = TRUE
  )

  list(features = features, chains = chains)
}


#' @rdname make_receptor_schema
#' @export
assert_receptor_schema <- function(schema) {
  # TODO: globals.R with schema list

  if (checkmate::test_character(schema, min.len = 1, any.missing = FALSE)) {
    schema <- make_receptor_schema(features = schema)
  } else {
    checkmate::assert_list(schema, len = 2, null.ok = FALSE)
    checkmate::assert_names(
      names(schema),
      permutation.of = c("features", "chains")
    )
    checkmate::assert_character(
      schema[["features"]],
      min.len = 1,
      any.missing = FALSE
    )
    checkmate::assert_character(
      schema[["chains"]],
      min.len = 1,
      max.len = 2,
      any.missing = FALSE,
      null.ok = TRUE
    )
  }

  receptor_chains <- imd_receptor_chains(schema)

  if (!is.null(receptor_chains)) {
    # Validate chain syntax rules
    if (length(receptor_chains) > 2) {
      cli::cli_abort("Schema can have at most 2 chain elements. Found {length(receptor_chains)}: [{paste(receptor_chains, collapse=', ')}]")
    }

    if (length(receptor_chains) >= 1) {
      # Check first chain doesn't contain pipe
      if (grepl("\\|", receptor_chains[1])) {
        cli::cli_abort("The first chain in the schema cannot contain '|' character. Found: '{receptor_chains[1]}'. The OR syntax is only allowed in the second chain.")
      }
    }

    if (length(receptor_chains) == 2) {
      # Check if second chain contains OR syntax (|)
      if (grepl("\\|", receptor_chains[2])) {
        # Split and validate the alternatives
        relaxed_chain_alternatives <- trimws(unlist(strsplit(receptor_chains[2], "\\|")))

        # Validate the alternatives
        if (length(relaxed_chain_alternatives) != 2) {
          cli::cli_abort("Relaxed pairing syntax requires exactly 2 alternatives separated by '|'. Found {length(relaxed_chain_alternatives)} in '{receptor_chains[2]}'")
        }

        # Check for empty alternatives
        if (any(relaxed_chain_alternatives == "")) {
          cli::cli_abort("Empty chain name found in '{receptor_chains[2]}'. Both alternatives must be valid chain names.")
        }

        # Check for duplicate alternatives
        if (length(unique(relaxed_chain_alternatives)) != length(relaxed_chain_alternatives)) {
          cli::cli_abort("Duplicate chain names found in '{receptor_chains[2]}'. Alternatives must be different.")
        }

        # Check that alternatives are different from the required chain
        if (receptor_chains[1] %in% relaxed_chain_alternatives) {
          cli::cli_abort("The required chain '{receptor_chains[1]}' cannot also be an alternative in '{receptor_chains[2]}'")
        }
      } else {
        # Strict pairing - check for accidental spaces or typos
        if (grepl("[\\s,;]", receptor_chains[2])) {
          cli::cli_warn("Found potential separator characters in '{receptor_chains[2]}'. For relaxed pairing, use the pipe character '|' to separate alternatives (e.g., 'IGL|IGK')")
        }
      }
    }
  }

  TRUE
}


#' @rdname make_receptor_schema
#' @export
test_receptor_schema <- function(schema) {
  isTRUE(tryCatch(
    suppressWarnings(assert_receptor_schema(schema)),
    error = function(...) FALSE
  ))
}
