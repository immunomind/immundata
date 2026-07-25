#' @title Compute ImmunData annotations
#'
#' @description
#' Materializes the annotation table of an `ImmunData` object via
#' [dplyr::compute()] and returns a new `ImmunData`.
#'
#' @param x ImmunData object.
#' @param ... Additional arguments passed to [dplyr::compute()] for
#'   `x$annotations`.
#'
#' @return A new `ImmunData` object with computed annotations and the input
#'   repertoire, strata, schema, and provenance state preserved.
#'
#' @concept operations
#' @exportS3Method dplyr::compute
compute.ImmunData <- function(x, ...) {
  checkmate::assert_r6(x, "ImmunData")

  new_annotations <- x$annotations |>
    compute(...)

  imd_clone_with_annotations(x, new_annotations)
}

#' @title Collect ImmunData annotations
#'
#' @description
#' Collects annotations from an `ImmunData` object and returns them as a tibble.
#' Factor columns are converted to character.
#'
#' @param x ImmunData object.
#' @param ... Additional arguments passed to [dplyr::collect()] for
#'   `x$annotations`.
#'
#' @return A tibble with collected annotations.
#'
#' @concept operations
#' @exportS3Method dplyr::collect
collect.ImmunData <- function(x, ...) {
  checkmate::assert_r6(x, "ImmunData")

  annotations <- x$annotations |>
    collect(...)

  if (is.data.frame(annotations)) {
    factor_cols <- vapply(annotations, is.factor, logical(1))
    if (any(factor_cols)) {
      annotations[factor_cols] <- lapply(annotations[factor_cols], as.character)
    }
  }

  as_tibble(annotations)
}
