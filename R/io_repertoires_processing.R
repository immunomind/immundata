#' @title Process chain rows while reading repertoire files
#'
#' @description
#' Use these functions to preprocess or postprocess rows of the input data before
#' returning the final `ImmunData` object to the session. A couple of example
#' use cases: keep productive receptor chains, remove technical
#' columns, or make cell barcodes unique while importing repertoire files with
#' [read_repertoires()].
#'
#' The defaults provide steps for common AIRR or 10x inputs. Use an individual step
#' when your files need only one operation or when you are building a custom
#' `preprocess` or `postprocess` list.
#'
#' Preprocessing changes chain rows before receptors are defined. Barcode
#' prefixing changes the cell identifier after receptor and manifest information
#' are combined. The input files and input table are not changed: every step
#' returns a new duckplyr table.
#'
#' @section Choose processing steps:
#'
#' * `make_default_preprocessing()` returns two steps. The first removes common
#'   technical columns. The second keeps rows whose `productive` value indicates
#'   a productive chain. If the `productive` column is absent, the filtering
#'   step gives a warning and keeps all rows.
#' * `make_default_postprocessing()` returns one step that adds a sample-specific
#'   prefix to cell barcodes. If the prefix column is absent, the step gives a
#'   warning and leaves barcodes unchanged.
#' * `make_exclude_columns()` creates one step that removes the columns in
#'   `cols`. Column names that are not present are ignored.
#' * `make_productive_filter()` creates one step that keeps rows whose value in
#'   `col_name` matches any value in `truthy`.
#' * `make_barcode_prefix()` creates one step that joins a prefix, such as
#'   `"Tumor_"`, to the start of each `imd_barcode` value.
#'
#' `read_repertoires()` applies functions in list order. You can therefore add,
#' remove, or reorder steps in a custom list.
#'
#' @section Input formats:
#'
#' For `make_default_preprocessing()`, `format = "default"` removes the union of
#' the standard AIRR and 10x technical columns. Use `format = "airr"` or
#' `format = "10x"` to remove only the columns expected for that format. All
#' three defaults recognize common text representations of a productive value,
#' including `"TRUE"`, `"true"`, `"yes"`, and `"1"`.
#'
#'
#' @param format A character string. One input format: `"default"`, `"airr"`,
#'   or `"10x"`. The default is `"default"`. This choice controls which
#'   technical columns are removed. It does not rename columns.
#' @param cols A character vector. Columns to remove. The default is
#'   `imd_drop_cols("airr")`. Use `character()` to create a step that removes
#'   no columns.
#' @param col_name A character string. Column containing the productive-chain
#'   indicator. The default is `"productive"`.
#' @param truthy A vector. Values that mean the chain is productive. Values are
#'   compared as text. The default is `TRUE`; use a character vector when the
#'   source uses several representations, for example
#'   `c("TRUE", "true", "1")`.
#' @param prefix_col A character vector. One or more candidate columns
#'   containing the text to place before each cell barcode. The first candidate
#'   present in the data is used. The default is `"Prefix"`.
#'
#' @return `make_default_preprocessing()` and
#'   `make_default_postprocessing()` return named lists of processing functions.
#'   The other functions return one processing function. Each processing
#'   function accepts a duckplyr table as its first argument, accepts unused
#'   arguments through `...`, and returns a new duckplyr table.
#'
#' @seealso [read_repertoires()], [imd_drop_cols()], [imd_rename_cols()]
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' # Three 10x chain rows from two samples. One chain is non-productive.
#' chains <- duckplyr::duckdb_tibble(
#'   imd_barcode = c("AAAC-1", "AAAG-1", "AATT-1"),
#'   cdr3_aa = c("CASSA", "CASSB", "CASSC"),
#'   productive = c("TRUE", "FALSE", "TRUE"),
#'   full_length = c(TRUE, TRUE, TRUE),
#'   Prefix = c("Tumor_", "Tumor_", "Blood_")
#' )
#'
#' # read_repertoires() performs these calls for you. They are shown here to
#' # make the effect of each list clear.
#' prepared <- Reduce(
#'   function(data, step) step(data),
#'   make_default_preprocessing("10x"),
#'   init = chains
#' )
#' prepared <- Reduce(
#'   function(data, step) step(data),
#'   make_default_postprocessing(),
#'   init = prepared
#' )
#'
#' prepared |>
#'   collect() |>
#'   select(imd_barcode, cdr3_aa, productive)
#' # Expected result:
#' #   imd_barcode  cdr3_aa productive
#' #   Tumor_AAAC-1 CASSA   TRUE
#' #   Blood_AATT-1 CASSC   TRUE
#'
#' # The non-productive chain was removed, `full_length` was dropped, and the
#' # sample prefixes made the retained cell barcodes unique.
#'
#' @concept processing
#' @rdname preprocess_postprocess
#' @export
make_default_preprocessing <- function(format = c("default", "airr", "10x")) {
  format <- match.arg(format)

  truthy <- c("TRUE", "True", "true", "T", "t", "YES", "Yes", "yes", "Y", "y", "1")

  if (format == "default") {
    list(
      exclude_columns = make_exclude_columns(imd_drop_cols("universal")),
      filter_nonproductive = make_productive_filter(truthy = truthy)
    )
  } else if (format == "airr") {
    list(
      exclude_columns = make_exclude_columns(imd_drop_cols("airr")),
      filter_nonproductive = make_productive_filter(truthy = truthy)
    )
  } else if (format == "10x") {
    list(
      exclude_columns = make_exclude_columns(imd_drop_cols("10x")),
      filter_nonproductive = make_productive_filter(truthy = truthy)
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
  checkmate::assert_string(col_name)

  fun <- function(dataset, ...) {
    col_name <- intersect(
      colnames(dataset),
      col_name
    )

    if (length(col_name) == 0) {
      cli::cli_alert_warning("No columns with the productive specification found; skipping the filtering")
      dataset
    } else {
      prod_col <- paste0("imd_", col_name)
      truthy <- truthy |> as.character()

      dataset <- dataset |> mutate(!!rlang::sym(prod_col) := dd$concat(!!rlang::sym(col_name), ""))

      if (length(truthy) == 1) {
        dataset <- dataset |> filter(!!rlang::sym(prod_col) == truthy)
      } else {
        dataset <- dataset |> filter(!!rlang::sym(prod_col) %in% truthy)
      }

      dataset |> select(-!!rlang::sym(prod_col))
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
      dataset |> mutate({{ barcode_col }} := dd$concat(
        !!rlang::sym(prefix_col),
        !!rlang::sym(barcode_col)
      ))
    } else {
      cli::cli_alert_warning("No column '{prefix_col}' with barcode prefixes found in the data; skipping the barcode processing")
      dataset
    }
  }

  fun
}
