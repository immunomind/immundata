#' @title Preprocessing and postprocessing of input immune repertoire files
#'
#' @concept Preprocessing and postprocessing
#' @rdname preprocess_postprocess
#' @export
make_default_preprocessing <- function(format = c("airr", "10x")) {
  format <- match.arg(format)

  if (format == "airr") {
    list(
      exclude_columns = make_exclude_columns(imd_drop_cols("airr")),
      filter_nonproductive = make_productive_filter(truthy = TRUE)
    )
  } else if (format == "10x") {
    list(
      exclude_columns = make_exclude_columns(imd_drop_cols("10x")),
      filter_nonproductive = make_productive_filter(truthy = c("true", "TRUE", "True", "t", "T","1"))
    )
  }
}


#' @rdname preprocess_postprocess
#' @export
make_default_postprocessing <- function() {
  list(
    prefix_barcodes = make_barcode_prefix()
  )
}


#' @rdname preprocess_postprocess
#' @export
make_exclude_columns <- function(cols = imd_drop_cols("airr")) {

  fun <- function(dataset, ...) {
    if (length(cols)) {
      dataset |>
        select(-any_of(cols))
    } else {
      dataset
    }
  }

  fun
}


#' @rdname preprocess_postprocess
#' @export
make_productive_filter <- function(col_name = c("productive"),
                                   truthy = TRUE) {
  checkmate::assert_character(col_name)

  fun <- function(dataset, ...) {
    col_name <- intersect(
      colnames(dataset),
      col_name
    )

    if (length(col_name) == 0) {
      cli::cli_alert_warning("No columns with productive specification found; skipping the filtering")
      dataset
    } else {
      col <- col_name[[1]]

      if (checkmate::test_logical(truthy)) {
        dataset |> filter(!!rlang::sym(col_name) == truthy)
      } else {
        dataset |> filter(!!rlang::sym(col_name) %in% truthy)
      }
    }
  }

  fun
}


#' @rdname preprocess_postprocess
#' @export
make_barcode_prefix <- function(prefix_col = "Prefix") {
  checkmate::assert_character(prefix_col)

  fun <- function(dataset, ...) {

    prefix_col_found <- intersect(prefix_col, colnames(dataset))[1]
    if (!is.na(prefix_col_found)) {
      barcode_col <- imd_schema("barcode")
      prefix_col <- prefix_col_found
      dataset |> mutate( {{ barcode_col }} := dd$concat(!!rlang::sym(prefix_col),
                                                         !!rlang::sym(barcode_col)))
    } else {
      cli::cli_alert_warning("No column '{prefix_col}' with barcode prefixes found in the data; skipping the barcode processing")
      dataset
    }
  }

  fun
}
