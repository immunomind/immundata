#' @title Write an ImmunData Object to Disk as Parquet Files
#'
#' @description
#' `write_immundata()` takes an [ImmunData] object and serializes its
#' receptor and annotation tables to two Parquet files in a specified
#' directory, using Zstandard compression for efficient storage and retrieval.
#'
#' @param idata [ImmunData] object containing the receptor and annotation tables.
#' @param output_folder Character(1). Path to the directory where the Parquet
#'   files will be written.  If the directory does not exist, it will be
#'   created (recursively).
#'
#' @details
#' The function performs the following steps:
#' 1. Validates that `idata` is an `ImmunData` object and that
#'    `output_folder` is a single non-`NULL` string.
#' 2. Normalizes and creates `output_folder` (if needed).
#' 3. Determines the filenames for the receptor and annotation tables
#'    via `imd_files()$receptors` and `imd_files()$annotations`.
#' 4. Writes `idata$receptors` and `idata$annotations` as Parquet files
#'    to those paths, using Zstandard compression (`compression = "zstd"`)
#'    at level 9 for a good balance of speed and file size.
#' 5. Emits informative CLI messages before and after writing each file.
#'
#' @return
#' Invisibly returns the input [ImmunData]. The primary side effect is the creation of
#' two Parquet files in `output_folder`.
#'
#' @examples
#' \dontrun{
#' # Suppose `idata` is an ImmunData object you constructed earlier:
#' outdir <- tempfile("immundata_")
#' write_immundata(idata, outdir)
#' # Now you can inspect:
#' list.files(outdir)
#' }
#'
#' @seealso [read_repertoires()], [read_immundata()], [ImmunData]
#'
#' @export
write_immundata <- function(idata, output_folder) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(output_folder,
    max.len = 1,
    null.ok = FALSE
  )

  output_folder <- normalizePath(output_folder, mustWork = FALSE)
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  receptor_path <- file.path(output_folder, imd_files()$receptors)
  annotations_path <- file.path(output_folder, imd_files()$annotations)

  receptor_path <- file.path(output_folder, imd_files()$receptors)
  annotations_path <- file.path(output_folder, imd_files()$annotations)

  cli_alert_info("Writing the receptor data to [{receptor_path}]")
  compute_parquet(idata$receptors,
    receptor_path,
    options = list(
      compression = "zstd",
      compression_level = 9
    )
  )

  cli_alert_info("Writing the annotation data to [{annotations_path}]")
  compute_parquet(idata$annotations,
    annotations_path,
    options = list(
      compression = "zstd",
      compression_level = 9
    )
  )

  cli_alert_success("ImmunData files saved to [{output_folder}]")

  invisible(idata)
}
