get_test_idata <- function() {
  sample_files <- c(
    system.file("extdata", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )
  read_repertoires(
    path          = sample_files,
    schema        = c("cdr3_aa", "v_call"),
    output_folder = tempfile()
  )
}

get_test_idata_with_metadata <- function() {
  md_path <- system.file("extdata", "metadata_samples.tsv", package = "immundata")

  sample_files <- c(
    system.file("extdata", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )
  read_repertoires(
    path          = sample_files,
    schema        = c("cdr3_aa", "v_call"),
    md,
    output_folder = tempfile()
  )
}
