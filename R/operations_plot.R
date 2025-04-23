#'
#' @export
autoplot.ImmunData <- function(idata,
                               y = c(imd_schema("n_barcodes"), imd_schema("n_receptors")),
                               x = imd_schema("repertoire"),
                               by = NULL,
                               ...) {
  y <- match.arg(y)

  pkgs <- c("ggplot2", "ggthemes", "ggsci")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    cli_abort("Missing packages: ", paste(missing, collapse = ", "))
  }

  repertoires <- idata$repertoires |> collect()
  repertoires$meta <- c("A", "A", "B")
  ggplot2::ggplot(data = repertoires) +
    ggplot2::geom_col(ggplot2::aes(x = meta,
                                   y = !!rlang::sym(y),
                                   group = meta,
                                   fill = as.factor(!!rlang::sym(x))),
             color = "black", position = "dodge") +
    ggthemes::theme_few()
}

fortify.ImmunData <- function(idata) {
  idata$repertoires |> collect() |> as_tibble()
}

#'
#' @export
vis <- function(idata, ...) {
  UseMethod("vis", idata)
}

#'
#' @export
vis.ImmunData <- autoplot.ImmunData
