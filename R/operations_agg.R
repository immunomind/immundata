#' @title Define repertoires by aggregating annotation fields for ImmunData
#'
#' @description
#' Groups the annotation table of an **ImmunData** object by one or more
#' user‑supplied columns that together define a *repertoire* (e.g. sample,
#' donor + time‑point).
#' For each repertoire it computes:
#'
#' * **`n_receptors`** – number of unique receptors in the repertoire
#' * **`n_cells`**     – total cell/UMI counts (`imd_count`) in the repertoire
#'
#' For every receptor it also adds:
#'
#' * **`n_repertoires`** – how many distinct repertoires this receptor occurs in
#' * **`imd_proportion`**      – proportion of its repertoire occupied by the receptor
#'
#' All new columns are appended to the annotation table; a repertoire summary
#' table is returned in the new object’s `repertoires` slot.
#'
#' @param idata  An [ImmunData] object.
#' @param schema Character vector of column names that, taken together, define a
#'   repertoire (default `"repertoire_id"`).  Can be length > 1 for composite
#'   repertoires.
#'
#' @details
#' The function first summarises `idata$annotations` to obtain per‑repertoire
#' and per‑receptor counts, then left‑joins the results back:
#'
#' 1. **Per‑repertoire totals** (`repertoires_table`)
#'    `n_receptors = n()`
#'    `n_cells     = sum(imd_count)`
#'
#' 2. **Receptor repertoire‑count** (`n_repertoires`) – distinct combinations
#'    of `schema` and `imd_receptor_id`.
#'
#' 3. **Per‑repertoire receptor proportion** (`imd_prop`) – receptor cells /
#'    total cells inside that repertoire.
#'
#' The receptor table itself is **not** modified; only the annotation table
#' gains the new columns.  A fresh [ImmunData] object is returned so downstream
#' pipelines remain lazy and chainable.
#'
#' @return A **new** [ImmunData] object whose `annotations` contain four extra
#'   columns (`n_repertoires`, `n_receptors`, `n_cells`, `imd_prop`) and whose
#'   `repertoires` slot holds the per‑repertoire summary table.
#'
#' @seealso [filter_receptors()], [annotate_receptors()]
#' @importFrom dplyr summarise distinct left_join select mutate n
#' @export

agg_repertoires <- function(idata, schema = "repertoire_id") {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(schema, min.len = 1)

  missing_cols <- setdiff(schema, colnames(idata$annotations))
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns in `annotations`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  receptor_id <- imd_schema()$receptor
  repertoire_schema_sym <- to_sym(schema)
  prop_col <- imd_schema()$proportion
  imd_count_col <- imd_schema()$count

  repertoires_table <- idata$annotations |>
    summarise(
      .by = schema,
      n_receptors = n(),
      n_cells = sum(!!to_sym(imd_count_col))
    )

  #
  # n_repertoires
  #
  repertoire_counts <- idata$annotations |>
    select(all_of(c(receptor_id, schema))) |>
    distinct() |>
    summarise(.by = receptor_id, n_repertoires = n())
  new_annotations <- idata$annotations |> left_join(repertoire_counts, by = receptor_id)

  #
  # proportions
  #
  receptor_cells <- new_annotations |> summarise(
    .by = c(schema, receptor_id),
    tmp_receptor_cells = sum(!!to_sym(imd_count_col))
  )

  receptor_props <- receptor_cells |>
    left_join(repertoires_table, by = schema) |>
    mutate({{ prop_col }} := tmp_receptor_cells / n_cells) |>
    select(-tmp_receptor_cells, -n_cells)

  new_annotations <- new_annotations |>
    left_join(receptor_props, by = c(schema, receptor_id))

  ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    repertoires = repertoires_table
  )
}
