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

  new_receptors <- idata$receptors |> semi_join(receptor_ids, by = receptor_id_col)

  ImmunData$new(receptors = new_receptors, annotations = new_annotations, schema = idata$schema_receptor)
}

#' @importFrom checkmate assert
#' @importFrom duckplyr duckdb_tibble
#' @importFrom dplyr distinct
#'
#' @title Filter ImmunData by Passed Barcodes
#'
#' @description
#' A short description...
#'
#' @param idata [ImmunData] object.
#' @param barcodes Vector of barcodes to filter by.
#'
#' @export
filter_barcodes <- function(idata, barcodes) {
  # TODO: Figure out what are the options to pass the barcodes.
  # - character vector
  # - something from Seurat / AnnData?

  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert(
    checkmate::check_character(barcodes, min.len = 1),
    checkmate::check_integer(barcodes, min.len = 1),
    checkmate::check_double(barcodes, min.len = 1)
  )

  barcode_col_id <- imd_schema()$barcode
  barcodes_table <- duckdb_tibble(A = unique(barcodes))
  colnames(barcodes_table) <- barcode_col_id

  new_annotations <- idata$annotations |> semi_join(barcodes_table, by = barcode_col_id)

  receptor_id_col <- imd_schema()$receptor
  receptor_ids <- new_annotations |>
    select({{ receptor_id_col }}) |>
    distinct(!!rlang::sym(receptor_id_col))

  new_receptors <- idata$receptors |> semi_join(receptor_ids, by = receptor_id_col)

  ImmunData$new(receptors = new_receptors, annotations = new_annotations, schema = idata$schema_receptor)
}
