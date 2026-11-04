#' @title Keep the most abundant receptors in ImmunData
#'
#' @description
#' Use `top_n()` to select abundant receptors for further analysis. For example,
#' keep the three most abundant receptors in each sample to compare their
#' sequences or cell annotations.
#'
#' If repertoires are defined, selection is performed separately in each
#' repertoire. Otherwise, the whole dataset is treated as one group.
#' The function returns a new [ImmunData] object. The original object is not
#' changed.
#'
#' @details
#' All observations belonging to a selected receptor in a selected repertoire
#' are kept, including both chains of a paired receptor. A receptor selected in
#' one repertoire is kept in another repertoire only if it is also selected
#' there.
#'
#' Without repertoire definitions, counts are calculated across the whole input
#' for selection. Each cell contributes once, and bulk observations contribute
#' their abundance values. No repertoire definitions are added to the result.
#'
#' When repertoires are defined, selection uses the input counts or proportions.
#' Repertoire and strata summaries are then recalculated for the selected data,
#' as in [filter_immundata()]. Returned proportions describe each receptor's
#' share of its filtered repertoire and can be larger than the input values.
#'
#' With `with_ties = FALSE`, the default, at most `n` receptors are kept per
#' repertoire, or in total when repertoires are not defined. Receptor identifiers
#' determine which equally abundant receptors are kept. Set `with_ties = TRUE`
#' to include all receptors with the same abundance at the selection limit.
#'
#' @param idata An [ImmunData] object.
#' @param n A non-negative whole number. How many receptors to keep in each
#'   repertoire, or in the whole dataset when repertoires are not defined.
#'   The default is `1L`. Set `n = 0` for an empty result. If fewer receptors are
#'   available, all available receptors are kept.
#' @param order_by A character string. Use `"imd_count"`, the default, to select
#'   by the number of cells or bulk sequence counts. Use `"imd_proportion"` to
#'   select by each receptor's fraction of its repertoire, or of the whole
#'   dataset when repertoires are not defined.
#' @param with_ties A logical value. If `FALSE`, the default, keep at most `n`
#'   receptors per group. If `TRUE`, keep all receptors tied at the selection
#'   limit, which can return more than `n` receptors.
#'
#' @return A new [ImmunData] object containing all observations of the selected
#'   receptors. Existing repertoire and strata summaries are recalculated for
#'   the selected data.
#'
#' @seealso [filter_immundata()], [agg_repertoires()], [ImmunData]
#'
#' @concept filtering
#'
#' @examples
#' library(immundata)
#'
#' options(immundata.verbose = FALSE)
#' idata <- get_test_idata()
#'
#' # Keep up to three receptors with the highest counts in each repertoire
#' top_receptors <- top_n(idata, n = 3L)
#'
#' top_receptors$repertoires |>
#'   select(imd_repertoire_id, n_receptors)
#'
#' # Include all receptors tied with the third receptor in each repertoire
#' top_with_ties <- top_n(idata, n = 3L, with_ties = TRUE)
#'
#' @export
top_n <- function(idata, n = 1L, order_by = "imd_count", with_ties = FALSE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_count(n)
  checkmate::assert_choice(order_by, c("imd_count", "imd_proportion"))
  checkmate::assert_flag(with_ties)

  ann <- idata$annotations
  groups <- if (is.null(idata$schema_repertoire)) character() else "imd_repertoire_id"
  keys <- c(groups, "imd_receptor_id")
  if (length(groups)) {
    ranked <- ann |>
      dplyr::select(dplyr::all_of(c(keys, order_by))) |>
      dplyr::distinct()
  } else {
    units <- intersect(c("imd_filename", "imd_barcode", "imd_receptor_id"), names(ann))
    ranked <- ann |>
      dplyr::summarise(.weight = dplyr::first(imd_n_chains), .by = dplyr::all_of(units)) |>
      dplyr::summarise(imd_count = sum(.weight, na.rm = TRUE), .by = imd_receptor_id)
    if (order_by == "imd_proportion") {
      ranked <- ranked |>
        dplyr::mutate(imd_proportion = imd_count / sum(imd_count, na.rm = TRUE))
    }
  }
  score <- rlang::sym(order_by)
  ranked <- ranked |>
    dplyr::arrange(dplyr::desc(!!score), !!!rlang::syms(keys)) |>
    dplyr::mutate(.rank = dplyr::row_number(), .by = dplyr::all_of(groups))
  if (with_ties) {
    ranked <- ranked |>
      dplyr::mutate(.rank = min(.rank, na.rm = TRUE),
                    .by = dplyr::all_of(c(groups, order_by)))
  }
  marker <- ".imd_top_selected"
  while (marker %in% names(ann)) marker <- paste0(marker, "_")
  selected <- ranked |>
    dplyr::filter(.rank <= !!n) |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::mutate(!!marker := TRUE)
  marked <- annotate_immundata(idata, selected,
                               by = stats::setNames(keys, keys), remove_limit = TRUE)
  filtered <- filter_immundata(marked, !!rlang::sym(marker))
  clone_with_annotations(filtered, filtered$annotations |> dplyr::select(-dplyr::all_of(marker)))
}
