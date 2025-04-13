#' @title Define repertoires by aggregating annotation fields for ImmunData
#'
#' @description Aggregates receptors to repertoires by selected annotating columns.
#'
#' @param idata [ImmunData] object.
#' @param schema A character vector of column names in `.annotations` used to define repertoire grouping.
#' @param sep Currently unused. A string separator used to concatenate multiple columns into a single repertoire ID.
#'
#' @export
agg_repertoires <- function(idata, schema = "repertoire_id", sep = "-") {
  checkmate::assert_r6(idata)
  checkmate::assert_character(schema, min.len = 1)
  checkmate::assert_character(sep, min.len = 1, max.len = 1)

  missing_cols <- setdiff(schema, colnames(idata$annotations))
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns in `annotations`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # TODO: passed by reference? What would happen if we change the `idata` after (!) creating the new ImmunData?
  # TODO: pass the schema column names here
  imd_count_col <- imd_schema()$count
  repertoires_table <- idata$annotations |> summarise(
    .by = {{ schema }},
    n_receptors = n(),
    n_cells = sum(!!rlang::sym(imd_count_col))
  )

  # TODO: recompute proportions
  # new_proportions <- idata$annotations |> summarise(.by = receptor_id + schema_repertoire, {{ imd_proportion_col }} := )

  ImmunData$new(
    receptors = idata$receptors,
    annotations = idata$annotations,
    schema = idata$schema_receptor,
    repertoires = repertoires_table
  )
}
