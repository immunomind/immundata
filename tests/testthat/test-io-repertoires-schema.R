test_that("read_repertoires() errors when both barcode_col and count_col are set", {
  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = c("cdr3_aa", "v_call"),
      barcode_col = "barcode",
      count_col = "count_col" # Not actually in the file, but we want the code path tested
    ),
    "Undefined case"
  )
})

test_that("read_repertoires() fails if missing columns in the receptor schema", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  bad_schema <- c("cdr3_aa", "v_call", "some_missing_col")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = bad_schema,
      output_folder = output_dir
    ),
    "Not all columns in the receptor schema present in the data"
  )
})
