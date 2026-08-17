#' Get test datasets from `immundata`
#' @keywords internal
#' @export
get_test_idata <- function() {
  get_test_idata_tsv_with_manifest()
}

#' Get test datasets from `immundata`
#' @keywords internal
#' @export
get_test_immundata <- function(repertoire_schema = "<auto>") {
  get_test_idata_tsv_with_manifest(repertoire_schema = repertoire_schema)
}

get_test_idata_tsv_with_manifest <- function(schema = c("cdr3_aa", "v_call"),
                                             repertoire_schema = "<auto>") {
  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest <- read_manifest(manifest_path)

  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )
  read_repertoires(
    path = sample_files,
    schema = schema,
    manifest = manifest,
    repertoire_schema = repertoire_schema,
    output_folder = tempfile()
  )
}
