#' @title Count the number of chains in ImmunData
#'
#' @param x ImmunData object.
#' @param ... Not used.
#' @param wt Not used.
#' @param sort Not used.
#' @param name Not used.
#'
#' @concept operations
#'
#' @exportS3Method dplyr::count
count.ImmunData <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  checkmate::check_r6(x, "ImmunData")

  x$annotations |> count()
}
