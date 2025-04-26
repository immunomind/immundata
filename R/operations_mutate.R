#' @exportS3Method dplyr::mutate
mutate.ImmunData <- function(idata,
                             ...,
                             seq_options = NULL,
                             keep_repertoires = TRUE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_list(seq_options, null.ok = TRUE)
  checkmate::assert_logical(keep_repertoires)

  ## ── check for reserved column names ─────────────────────────────────────────
  dots <- rlang::enquos(..., .named = TRUE) # keep names exactly as passed
  bad <- names(dots)[startsWith(names(dots), "imd_")]

  if (length(bad)) {
    cli::cli_abort(
      "You cannot create or overwrite system columns that start with {.code imd_}. ",
      "Offending names: {.val {bad}}"
    )
  }

  ## … continue with the actual mutate logic …
}
