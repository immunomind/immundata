#' @title Count chain rows in ImmunData
#'
#' @description
#' Use `count()` to find how many chain rows are stored in an [ImmunData]
#' object.
#'
#' Use this method for a quick check of dataset size. The unit counted is one
#' retained chain row. Each retained cell with a paired receptor usually
#' contributes two rows, one for each chain. The same receptor can therefore
#' contribute two rows for every cell carrying it. For bulk data with an
#' abundance column, this method counts table rows rather than the summed
#' sequence abundance.
#'
#' The function returns a one-row duckplyr table. The original object is not
#' changed.
#'
#' @details
#' This method currently provides only the total row count. The grouping,
#' weighting, sorting, and result-name arguments of [dplyr::count()] are
#' accepted for method compatibility but are not applied.
#'
#' The calculation runs on the duckplyr annotation table and can remain in
#' DuckDB. Use [dplyr::pull()] or [dplyr::collect()] to bring the small result
#' into R.
#'
#' @param x An [ImmunData] object.
#' @param ... Additional arguments. Accepted for compatibility with
#'   [dplyr::count()], but currently ignored.
#' @param wt Any value or `NULL`. Accepted for compatibility with
#'   [dplyr::count()], but currently ignored.
#' @param sort A logical value. Accepted for compatibility with
#'   [dplyr::count()], but currently ignored.
#' @param name A character string or `NULL`. Accepted for compatibility with
#'   [dplyr::count()], but currently ignored. The result column is always named
#'   `n`.
#'
#' @return A one-row duckplyr table with an integer column named `n`. This value
#'   is the number of rows in the chain-level annotation table.
#'
#' @seealso [dplyr::count()], [dplyr::collect()], [ImmunData]
#'
#' @examplesIf identical(Sys.getenv("IMD_RUN_EXAMPLES"), "true")
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#' idata <- get_test_idata()
#'
#' idata |> count()
#' # Expected result:
#' #      n
#' #   1902
#'
#' # The result means that the object contains 1,902 retained chain rows.
#' # It does not mean that it contains 1,902 unique receptors.
#'
#' @concept operations
#' @exportS3Method dplyr::count
count.ImmunData <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  checkmate::assert_r6(x, "ImmunData")

  x$annotations |> count()
}
