#' @title Get Annotation Dimnames from ImmunData
#'
#' @description
#' Returns dimension names for an `ImmunData` object so that
#' `colnames(idata)` maps to annotation column names.
#'
#' @param x ImmunData object.
#'
#' @return A list with `NULL` row names and annotation column names.
#'
#' @concept operations
#' @export
dimnames.ImmunData <- function(x) {
  checkmate::assert_r6(x, "ImmunData")

  list(NULL, colnames(x$annotations))
}

#' @title Prevent Renaming ImmunData via dimnames
#'
#' @description
#' Disallows replacing dimension names on `ImmunData`.
#'
#' @param x ImmunData object.
#' @param value Not used.
#'
#' @concept operations
#' @exportS3Method base::`dimnames<-` ImmunData
`dimnames<-.ImmunData` <- function(x, value) {
  checkmate::assert_r6(x, "ImmunData")

  cli::cli_abort(
    "Renaming columns via {.code colnames<-} or {.code dimnames<-} is not allowed for {.code ImmunData}."
  )
}

#' @title Prevent Renaming ImmunData via names
#'
#' @description
#' Disallows replacing names on `ImmunData`.
#'
#' @param x ImmunData object.
#' @param value Not used.
#'
#' @concept operations
#' @exportS3Method base::`names<-` ImmunData
`names<-.ImmunData` <- function(x, value) {
  checkmate::assert_r6(x, "ImmunData")

  cli::cli_abort(
    "Renaming via {.code names<-} is not allowed for {.code ImmunData}."
  )
}
