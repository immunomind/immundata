#' @title Save an ImmunData object to disk
#'
#' @description
#' Save `ImmunData` to disk so you can close R and continue the work later (I cannot
#' believe it, but it works, I tried it). Use
#' `write_immundata()` after importing or transforming repertoire data, or when
#' you want a named snapshot before the next analysis step.
#'
#' The unit saved is the complete [ImmunData] object. This includes retained
#' chain rows, cell and receptor identifiers, repertoire and stratum definitions,
#' and provenance. Saving does not add, remove, or change any biological unit.
#'
#' @param idata An [ImmunData] object you want to save.
#' @param output_folder A character string or `NULL`. Directory in which to
#'   write `annotations.parquet` and `metadata.json`. If `NULL`, the default, a
#'   managed snapshot is created at `home_path/snapshots/<tag>/vNNN`. The home
#'   path comes from the object's provenance.
#' @param tag A character string or `NULL`. Snapshot tag. With
#'   `output_folder = NULL`, it names the managed snapshot series; if `tag` is
#'   also `NULL`, `"default"` is used. With an explicit `output_folder`, a
#'   supplied tag is recorded in the lineage but does not change the output
#'   path.
#' @param rehome A logical value. Whether an explicit `output_folder` becomes
#'   the home for future managed snapshots. The default is `FALSE`, which
#'   preserves an existing home. If the object has no home yet, its first
#'   explicit output folder becomes the home with either value. `TRUE` requires
#'   an explicit `output_folder`.
#' @param compression A character string or `NULL`. Parquet compression codec
#'   passed to DuckDB. The default is `"zstd"`. Use `NULL` to let DuckDB choose.
#' @param compression_level A number or `NULL`. Compression level for codecs
#'   that support it. The default is `9`. Use `NULL` to let DuckDB choose.
#' @param verbose A logical value. Whether to print progress and summary
#'   messages. Defaults to `getOption("immundata.verbose", TRUE)`.
#'
#' @details
#' Save to an explicit folder for a direct saved state, or use the object's home
#' to create a versioned managed snapshot.
#'
#' @section Choose how to save:
#'
#' Supply `output_folder` to save a standalone state in a specific directory.
#' This is useful when sharing a dataset or choosing its first project home.
#' If the directory already contains an ImmunData dataset, its
#' `annotations.parquet` and `metadata.json` are replaced.
#'
#' Leave `output_folder = NULL` to create a managed snapshot. The function uses
#' the object's home path and writes the next version under
#' `snapshots/<tag>/vNNN`, for example `snapshots/baseline/v001`. Later writes
#' with the same tag create `v002`, `v003`, and so on; earlier versions remain
#' available. Use [read_immundata()] with `tag` and `version` to reopen one.
#'
#' Every save receives a new snapshot identifier and appends a provenance event.
#' The returned object records the new saved directory as its current path.
#'
#' @section Backend and serialization:
#'
#' The retained chain-level annotation table is materialized as compressed
#' `annotations.parquet`. Materialization executes any pending lazy duckplyr
#' calculations. `metadata.json` serializes format and package versions,
#' receptor, repertoire, and stratum schemas, the small repertoire table, the
#' snapshot identifier, lineage events, and provenance paths.
#'
#' Receptor and stratum views are not written as separate files; they can be
#' reconstructed from the annotation table and metadata. This Parquet and JSON
#' pair is an ImmunData-specific serialization, not an RDS file.
#'
#' @return Invisibly returns a newly reopened, disk-backed [ImmunData] object
#'   with provenance for the new save. The input `idata` remains unchanged.
#'
#' @seealso [read_immundata()] for continuing a saved analysis,
#'   [read_repertoires()] for importing AIRR-seq files, [ImmunData]
#'
#' @concept ingestion
#' @export
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Save a small immune-repertoire analysis
#' idata <- get_test_idata()
#' save_dir <- tempfile("saved-immundata-")
#'
#' saved_idata <- write_immundata(idata, save_dir)
#'
#' list.files(save_dir)
#' # Expected result: the analysis is serialized as two files.
#' # [1] "annotations.parquet" "metadata.json"
#'
#' # Continue the analysis from the saved files
#' continued_idata <- read_immundata(save_dir)
#'
#' continued_idata |>
#'   collect() |>
#'   summarise(
#'     n_chains = n(),
#'     n_receptors = n_distinct(imd_receptor_id)
#'   )
#' # Expected result: all 1,902 chain rows and 1,668 receptors are restored.
#' #   n_chains n_receptors
#' #       1902        1668
#'
#' unlink(save_dir, recursive = TRUE)
write_immundata <- function(idata,
                            output_folder = NULL,
                            tag = NULL,
                            rehome = FALSE,
                            compression = "zstd",
                            compression_level = 9,
                            verbose = getOption("immundata.verbose", TRUE)) {
  write_immundata_internal(
    idata = idata,
    output_folder = output_folder,
    snapshot_tag = tag,
    rehome = rehome,
    compression = compression,
    compression_level = compression_level,
    producer_function = "write_immundata",
    verbose = verbose
  )
}
