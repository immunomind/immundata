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
    schema <- x$schema_receptor
    # TODO: improve this
    for (val in schema) {
      if (!(val %in% imd_schema())) {
        cli::cli_bullets(c(">" = val))
      }
    }
  }

  if (!is.null(x$schema_repertoire)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_magenta('Repertoire schema:')}")
    schema <- x$schema_repertoire
    names(schema) <- rep(">", times = length(schema))
    cli::cli_bullets(schema)
  }

  if (!is.null(x$repertoires)) {
    cli::cat_line()
    cli::cli_h2("{cli::col_br_cyan('List of repertoires:')}")
    print(x$repertoires)
  }

  invisible(x)
}
