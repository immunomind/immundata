#' @export
annotate_receptors <- function(idata, annotations, annot_receptor_id_col = imd_schema()$receptor) {
  # Check if it is duckplyr already and convert if necessary

  # Rename the columns to follow ImmunData's schema

  # Join

  # TODO: Check NAs for missing values
  # TODO: Check if the old immundata still holds the old table
}

#' @export
annotate_cells <- function(idata, annotations, annot_cell_id_col = imd_schema()$cell) {
  # Well, well, well, would you look at that... Decided to dump all tens of thousands genes into your repertoire data?
  # I mean, sure, do whataver you want. But I'm not responsible for the freezes or crashes.
  # Pass `remove_limit = True` to `annotate_cells` to allow working with annotations of arbitrary size.
  # But remember: you have been warned.

  # Check if it is duckplyr already and convert if necessary

  # Rename the columns to follow ImmunData's schema

  # Join

  # TODO: Check NAs for missing values
  # TODO: Check if the old immundata still holds the old table
}
