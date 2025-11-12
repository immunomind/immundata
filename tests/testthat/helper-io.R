create_test_output_dir <- function(name = "test_immundata_") {
  tempfile(name)
}

cleanup_output_dir <- function(dir) {
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
}

test_ig_data <- function() {
  system.file("extdata/ig", "multiple_ig_loci.tsv.gz", package = "immundata")
}
