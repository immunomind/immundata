#' @importFrom dplyr semi_join filter n
#'
#' @title Filter ImmunData by Receptor Features
#'
#' @description
#' A short description...
#'
#' @param idata [ImmunData] object.
#' @param ... Parameters passed to [dplyr::filter].
#'
#' @export
filter_receptors <- function(idata, ...) {
  checkmate::assert_r6(idata, "ImmunData")

  new_receptors <- idata$receptors |> filter(...)

  receptor_id_col <- imd_schema()$receptor
  filtered_receptor_ids <- new_receptors |> select({{ receptor_id_col }})
  new_annotations <- idata$annotations |> semi_join(filtered_receptor_ids, by = receptor_id_col)

  # TODO: do I need to recompute counts / proportions each time?

  ImmunData$new(receptors = new_receptors, annotations = new_annotations, schema = idata$schema_receptor)
}

#' @title Filter ImmunData by Annotations
#'
#' @description
#' A short description...
#'
#' @param idata [ImmunData] object.
#' @param ... Parameters passed to [dplyr::filter].
#'
#' @export
filter_annotations <- function(idata, ...) {
  checkmate::assert_r6(idata, "ImmunData")

  new_annotations <- idata$annotations |> filter(...)

  receptor_id_col <- imd_schema()$receptor
  receptor_ids <- new_annotations |>
    select({{ receptor_id_col }}) |>
    distinct(!!rlang::sym(receptor_id_col))

  # TODO: do I need to recompute counts / proportions each time?

  new_receptors <- idata$receptors |> semi_join(receptor_ids, by = receptor_id_col)

  ImmunData$new(receptors = new_receptors, annotations = new_annotations, schema = idata$schema_receptor)
}

#' @importFrom checkmate assert
#' @importFrom duckplyr duckdb_tibble
#' @importFrom dplyr distinct
#'
#' @title Filter ImmunData by Passed Cell IDs or Barcodes
#'
#' @description
#' A short description...
#'
#' @param idata [ImmunData] object.
#' @param cells Vector of cell identifiers or barcodes to filter by.
#'
#' @export
filter_cells <- function(idata, cells) {
  # TODO: Figure out what are the options to pass the cells
  # - character vector
  # - something from Seurat / AnnData?

  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert(
    checkmate::check_character(cells, min.len = 1),
    checkmate::check_integer(cells, min.len = 1),
    checkmate::check_double(cells, min.len = 1)
  )

  barcode_col_id <- imd_schema()$cell
  barcodes_table <- duckdb_tibble(A = unique(cells))
  colnames(barcodes_table) <- barcode_col_id

  new_annotations <- idata$annotations |> semi_join(barcodes_table, by = barcode_col_id)

  # TODO: do I need to recompute proportions each time?

  receptor_id_col <- imd_schema()$receptor
  receptor_ids <- new_annotations |>
    select({{ receptor_id_col }}) |>
    distinct(!!rlang::sym(receptor_id_col))

  new_receptors <- idata$receptors |> semi_join(receptor_ids, by = receptor_id_col)

  ImmunData$new(receptors = new_receptors, annotations = new_annotations, schema = idata$schema_receptor)
}
