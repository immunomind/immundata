#' @title Create or validate a receptor schema object
#'
#' @description
#' Helper functions for defining and validating the `schema` used by
#' [agg_receptors()] to identify unique receptors.
#'
#' `make_receptor_schema()` creates a schema list object.
#' `assert_receptor_schema()` checks if an object is a valid schema list and throws
#'   an error if not.
#' `test_receptor_schema()` checks if an object is a valid schema list or a
#'   character vector (which `agg_receptors` can also accept) and returns `TRUE`
#'   or `FALSE`.
#'
#' @param features Character vector. Column names defining the features of a
#'   single receptor chain (e.g., V gene, J gene, CDR3 sequence).
#' @param chains Optional character vector (max length 2). Locus names (e.g.,
#'   `"TRA"`, `"TRB"`) to filter by or pair. If `NULL` or length 1, only
#'   filtering occurs. If length 2, pairing logic is enabled in [agg_receptors()].
#'   Default: `NULL`.
#' @param schema An object to test or assert as a valid schema. Can be a list
#'   created by `make_receptor_schema` or a character vector (for `test_receptor_schema`).
#'
#' @return
#' `make_receptor_schema` returns a list with elements `features` and `chains`.
#' `assert_receptor_schema` returns `TRUE` invisibly if valid, or stops execution.
#' `test_receptor_schema` returns `TRUE` or `FALSE`.
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

  checkmate::assert(
    checkmate::check_character(schema, min.len = 1),
    checkmate::check_list(schema, len = 2, null.ok = FALSE) &&
      checkmate::check_names(names(schema), must.include = c("features", "chains"))
  )

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
  checkmate::test_character(schema, min.len = 1) || (
    checkmate::test_list(schema, len = 2, null.ok = FALSE) &&
      checkmate::test_subset(names(schema), c("features", "chains"), empty.ok = FALSE)
  )
}
