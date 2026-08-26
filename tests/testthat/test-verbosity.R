test_that("read_repertoires can run quietly", {
  input_file <- system.file(
    "extdata/tsv",
    "sample_0_1k.tsv",
    package = "immundata"
  )
  output_dir <- create_test_output_dir("quiet_read_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  expect_silent(
    idata <- read_repertoires(
      path = input_file,
      schema = c("cdr3_aa", "v_call"),
      output_folder = output_dir,
      preprocess = NULL,
      postprocess = NULL,
      rename_columns = NULL,
      verbose = FALSE
    )
  )

  expect_s3_class(idata$annotations, "duckplyr_df")
})

test_that("the package verbosity option is used by default", {
  old_options <- options(immundata.verbose = FALSE)
  on.exit(options(old_options), add = TRUE)

  dataset <- duckplyr::duckdb_tibble(data.frame(
    cdr3_aa = c("CASSA", "CASSB")
  ))

  expect_silent(
    result <- agg_receptors(dataset, schema = "cdr3_aa")
  )

  expect_s3_class(result, "duckplyr_df")
})

test_that("verbose output remains the default behavior", {
  dataset <- duckplyr::duckdb_tibble(data.frame(
    cdr3_aa = c("CASSA", "CASSB")
  ))

  messages <- capture_messages(
    agg_receptors(dataset, schema = "cdr3_aa", verbose = TRUE)
  )

  expect_match(paste(messages, collapse = "\n"), "No locus information found")
})

test_that("manifest and snapshot I/O can run quietly", {
  manifest_path <- system.file(
    "extdata/tsv",
    "manifest.csv",
    package = "immundata"
  )

  expect_silent(
    manifest <- read_manifest(manifest_path, verbose = FALSE)
  )

  input_file <- manifest$file[[1]]
  root_dir <- create_test_output_dir("quiet_io_root_")
  snapshot_dir <- create_test_output_dir("quiet_io_snapshot_")
  on.exit(cleanup_output_dir(root_dir), add = TRUE)
  on.exit(cleanup_output_dir(snapshot_dir), add = TRUE)

  idata <- read_repertoires(
    path = input_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = root_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL,
    verbose = FALSE
  )

  expect_silent(
    written <- write_immundata(
      idata,
      output_folder = snapshot_dir,
      verbose = FALSE
    )
  )
  expect_silent(
    loaded <- read_immundata(snapshot_dir, verbose = FALSE)
  )

  expect_s3_class(written$annotations, "duckplyr_df")
  expect_s3_class(loaded$annotations, "duckplyr_df")
})
