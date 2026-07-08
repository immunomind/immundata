test_that("read_repertoires() errors when both barcode_col and count_col are set", {
  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = c("cdr3_aa", "v_call"),
      barcode_col = "barcode",
      count_col = "count_col" # Not actually in the file, but we want the code path tested
    ),
    "either .*barcode_col.*count_col"
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
    "Missing receptor feature column\\(s\\)"
  )
})

test_that("read_repertoires() fails with a clear message when barcode column is missing", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = make_receptor_schema(
        features = c("v_call", "j_call", "junction_aa"),
        chains = "IGH"
      ),
      barcode_col = "missing_barcode_col",
      locus_col = "locus",
      umi_col = "counts",
      output_folder = output_dir
    ),
    "Missing column\\(s\\) referenced by arguments: \\[missing_barcode_col\\]"
  )
})

test_that("read_repertoires() fails with a clear message when manifest file column is missing", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  manifest <- data.frame(
    WrongFileCol = "some/path.tsv",
    stringsAsFactors = FALSE
  )

  expect_error(
    read_repertoires(
      path = "<manifest>",
      manifest = manifest,
      manifest_file_col = "file",
      schema = c("cdr3_aa", "v_call"),
      output_folder = output_dir
    ),
    "has no column"
  )
})

test_that("read_repertoires() fails when single-cell mode is requested without umi_col", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = c("cdr3_aa", "v_call"),
      barcode_col = "sequence_id",
      output_folder = output_dir
    ),
    "Single-cell mode requires .*umi_col"
  )
})

test_that("read_repertoires() fails when chained schema is passed without locus_col", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = make_receptor_schema(
        features = c("cdr3_aa", "v_call"),
        chains = "IGH"
      ),
      output_folder = output_dir
    ),
    "locus_col.*NULL"
  )
})

test_that("read_repertoires() fails when paired schema is passed without barcode_col", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = make_receptor_schema(
        features = c("cdr3_aa", "v_call"),
        chains = c("IGH", "IGL")
      ),
      locus_col = "locus",
      output_folder = output_dir
    ),
    "barcode_col.*NULL"
  )
})

test_that("read_repertoires() fails with a clear message when count_col is missing", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = c("cdr3_aa", "v_call"),
      count_col = "missing_count_col",
      output_folder = output_dir
    ),
    "Missing column\\(s\\) referenced by arguments: \\[missing_count_col\\]"
  )
})

test_that("read_repertoires() fails with a clear message when locus_col is missing", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = make_receptor_schema(
        features = c("cdr3_aa", "v_call"),
        chains = "IGH"
      ),
      barcode_col = "sequence_id",
      locus_col = "missing_locus_col",
      umi_col = "counts",
      output_folder = output_dir
    ),
    "Missing column\\(s\\) referenced by arguments: \\[missing_locus_col\\]"
  )
})

test_that("read_repertoires() fails with a clear message when umi_col is missing", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = make_receptor_schema(
        features = c("cdr3_aa", "v_call"),
        chains = "IGH"
      ),
      barcode_col = "sequence_id",
      locus_col = "locus",
      umi_col = "missing_umi_col",
      output_folder = output_dir
    ),
    "Missing column\\(s\\) referenced by arguments: \\[missing_umi_col\\]"
  )
})

test_that("read_repertoires() fails when manifest paths are empty or NA", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  manifest <- data.frame(
    file = c("", NA),
    stringsAsFactors = FALSE
  )

  expect_error(
    read_repertoires(
      path = "<manifest>",
      manifest = manifest,
      schema = c("cdr3_aa", "v_call"),
      output_folder = output_dir
    ),
    "contains empty/NA paths"
  )
})
