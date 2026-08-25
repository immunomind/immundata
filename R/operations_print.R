#' @title Display the contents and biological definitions of ImmunData
#'
#' @description
#' Use `print()` to inspect the receptor table, chain annotations, and biological
#' schemas stored in an [ImmunData] object.
#'
#' Use this method for a quick overview after reading, filtering, or aggregating
#' repertoire data. It displays the units available in the object: receptors,
#' chain rows, repertoires, and strata. It also shows the feature and chain
#' definitions used to construct receptors.
#'
#' Printing is read-only. It does not collect the complete dataset into R and
#' does not change the original object. The object is returned invisibly so it
#' can still be assigned or used in a pipeline.
#'
#' @details
#' A section is shown only when that information is available. An object without
#' repertoire definitions, for example, has no repertoire schema or repertoire
#' summary section. Duckplyr prints a preview of large tables rather than every
#' row.
#'
#' @param x An [ImmunData] object to display.
#' @param ... Additional arguments. Currently not used.
#'
#' @return `x`, invisibly. The displayed output is a human-readable overview;
#'   no data are modified.
#'
#' @seealso [ImmunData], [dplyr::collect()], [dplyr::count()]
#'
#' @examplesIf identical(Sys.getenv("IMD_RUN_EXAMPLES"), "true")
#' library(immundata)
#'
#' options(immundata.verbose = FALSE)
#' idata <- get_test_idata()
#'
#' print(idata)
#' # Expected output contains these sections:
#' #   ImmunData
#' #   Receptors
#' #   Annotations
#' #   Receptor schema
#' #   Repertoire schema
#' #   List of repertoires
#'
#' # `Receptors` previews distinct biological receptor definitions.
#' # `Annotations` previews the retained chain rows and sample information.
#' # The schema sections explain how receptors and repertoires were defined.
#'
#' @concept operations
#' @export
print.ImmunData <- function(x, ...) {
  cli::cli_h1("{cli::col_blue('ImmunData')}")

  receptors <- x$receptors
  if (!is.null(receptors)) {
    cli::cli_h2("{cli::col_green('Receptors:')}")
    print(receptors)
  }

  if (!is.null(x$annotations)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_yellow('Annotations')}:")
    print(x$annotations)
  }

  if (!is.null(x$schema_receptor)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_red('Receptor schema:')}")
    # TODO: improve this
    cli::cli_text("features:")
    for (val in imd_receptor_features(x$schema_receptor)) {
      if (!(val %in% imd_schema())) {
        cli::cli_bullets(c(">" = val))
      }
    }
    cli::cli_text("chains:")
    if (length(imd_receptor_chains(x$schema_receptor))) {
      for (val in imd_receptor_chains(x$schema_receptor)) {
        if (!(val %in% imd_schema())) {
          cli::cli_bullets(c(">" = val))
        }
      }
    } else {
      cli::cli_text("~")
    }
  }

  if (!is.null(x$schema_repertoire)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_magenta('Repertoire schema:')}")
    schema <- x$schema_repertoire
    names(schema) <- rep(">", times = length(schema))
    cli::cli_bullets(schema)
  }

  if (!is.null(x$schema_strata)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_blue('Strata schema:')}")
    schema <- x$schema_strata
    names(schema) <- rep(">", times = length(schema))
    cli::cli_bullets(schema)
  }

  if (!is.null(x$repertoires)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_cyan('List of repertoires:')}")
    print(x$repertoires)
  }

  if (!is.null(x$strata)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_blue('List of strata:')}")
    print(x$strata)
  }

  invisible(x)
}
