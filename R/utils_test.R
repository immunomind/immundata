#' Get test datasets from `immundata`
#' @keywords internal
#' @export
get_test_idata <- function() {
  manifest_path <- system.file(
    "extdata/parquet",
    "manifest.csv",
    package = "immundata"
  )
  manifest <- read_manifest(manifest_path)

  sample_files <- c(
    system.file(
      "extdata/parquet",
      "sample_0_1k.parquet",
      package = "immundata"
    ),
    system.file(
      "extdata/parquet",
      "sample_1k_2k.parquet",
      package = "immundata"
    )
  )
  read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    manifest = manifest,
    output_folder = tempfile()
  )
}
