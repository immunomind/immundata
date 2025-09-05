get_test_idata_tsv_no_metadata <- function(schema = c("cdr3_aa", "v_call")) {
  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )
  read_repertoires(
    path          = sample_files,
    schema        = schema,
    output_folder = tempfile()
  )
}

get_test_idata_tsv_with_metadata <- function(schema = c("cdr3_aa", "v_call")) {
  md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
  md <- read_metadata(md_path)

  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )
  read_repertoires(
    path = sample_files,
    schema = schema,
    metadata = md,
    output_folder = tempfile()
  )
}

#' Get test datasets from `immundata`
#'
#' @export
get_test_idata <- function() {
  get_test_idata_tsv_with_metadata()
}

get_test_idata_tsv_metadata <- function() {
  md_path <- system.file("extdata/tsv", "metadata_samples.tsv", package = "immundata")

  read_metadata(md_path)
}
