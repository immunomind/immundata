#' @exportS3Method dplyr::count
count.ImmunData <- function(idata) {
  checkmate::check_r6(idata, "ImmunData")

  idata$annotations |> count()
}
