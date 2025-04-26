#' Convert an immunarch Object into an ImmunData Dataset
#'
#' @description
#' The `from_immunarch()` function takes an **immunarch** object (as returned by
#' `immunarch::repLoad()`), writes each repertoire to a TSV file with an added
#' `filename` column in a specified folder, and then imports those files into
#' an **ImmunData** object via `read_repertoires()`.
#'
#' @param imm A list returned by `immunarch::repLoad()`, typically containing:
#'   - **`data`**: a named list of `data.frame`s, one per repertoire.
#'   - **`meta`**: (optional) a `data.frame` of sample metadata.
#' @param schema Character vector of column names that together define unique
#'   receptors (for example, `c("CDR3.aa", "V.name", "J.name")`).
#' @param temp_folder Path to a directory where intermediate TSV files will
#'   be written. Defaults to `file.path(tempdir(), "temp_folder")`.
#' @param output_folder Path to the output directory where the resulting ImmunData
#'   Parquet files will be stored. This directory will be created if it does
#'   not already exist.
#'
#' @return An [ImmunData] object containing all repertoires from the input
#'   immunarch object, with data saved under `output_folder`.
#'
#' @seealso [read_repertoires()], [read_immundata()], [ImmunData]
#'
#' @examples
#' \dontrun{
#' imm <- immunarch::repLoad("/path/to/your/files")
#' idata <- from_immunarch(imm,
#'   schema = c("CDR3.aa", "V.name"),
#'   temp_folder = tempdir(),
#'   output_folder = "/path/to/immundata_out"
#' )
#' }
#'
#' @export
from_immunarch <- function(
    imm,
    output_folder,
    schema = c("CDR3.aa", "V.name"),
    temp_folder = file.path(tempdir(), "temp_folder")) {
  # validate inputs
  checkmate::assert_list(imm, any.missing = FALSE)
  checkmate::assert_character(schema, min.len = 1)
  checkmate::assert_character(temp_folder, len = 1)
  checkmate::assert_character(output_folder, len = 1)

  # ensure folders exist
  dir.create(temp_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  # extract the list of repertoires
  rep_list <- imm$data
  # if unnamed, give them default names
  if (is.null(names(rep_list))) {
    names(rep_list) <- paste0("repertoire_", seq_along(rep_list))
  }

  # write each repertoire with a 'filename' column
  immundata_filename_col <- IMD_GLOBALS$schema$filename
  file_paths <- c()
  for (nm in names(rep_list)) {
    df <- rep_list[[nm]]
    if (immundata_filename_col %in% colnames(df)) {
      cli::cli_alert_warning("{nm}: Removing `{immundata_filename_col}` columns from repertoires for compatability with `immundata`.")
      df[[immundata_filename_col]] <- NULL
    }
    if (!("repertoire_id" %in% colnames(df))) {
      cli::cli_alert_info("{nm}: Adding `repertoire_id` column for compatability with `immundata`.")
      df$repertoire_id <- nm
    } else {
      cli::cli_alert_info("{nm}: Found `repertoire_id` column; we will use it for compatability with `immundata`.")
    }
    out_path <- file.path(temp_folder, paste0(nm, ".tsv"))
    readr::write_tsv(df, out_path)
    file_paths <- c(file_paths, out_path)
  }
  names(file_paths) <- names(rep_list)

  # if metadata present, add 'filename' column and pass it in
  metadata_df <- NULL
  if (!is.null(imm$meta)) {
    metadata_df <- imm$meta
    if ("Sample" %in% colnames(metadata_df)) {
      metadata_df[[immundata_filename_col]] <- normalizePath(file_paths[metadata_df$Sample])
    } else {
      cli_abort("No `Sample` in the metadata object. Please create the column with repertoires names from `$data`")
    }
  } else {
    # TODO: Check at the beginning
    cli_abort("No `meta` object in the input list. Are you sure this is a correct `immunarch` dataset?")
  }

  # import into ImmunData via read_repertoires()
  read_repertoires(
    path = unname(file_paths),
    schema = schema,
    metadata = metadata_df,
    output_folder = output_folder,
    repertoire_schema = "repertoire_id"
  )
}
