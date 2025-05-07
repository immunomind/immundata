test_that("read_repertoires() fails if path doesn't exist", {
  expect_error(
    read_repertoires(path = "nonexistent_file.tsv", schema = c("cdr3_aa", "v_call")),
    "No file provided"
  )
})

test_that("read_repertoires() works with sample data and merges metadata", {
  # Load example data shipped with your package
  md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )

  # This function presumably reads the metadata file
  # (If you have a 'read_metadata()' or 'load_metadata()' function.)
  metadata_df <- read_metadata(md_path)

  outdir <- tempdir()

  # Run the function
  imdata <- read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    metadata = metadata_df,
    output_folder = outdir
  )

  # Basic check: Did we get an object back?
  expect_true(!is.null(imdata))

  checkmate::expect_r6(imdata, classes = "ImmunData")

  # Check if the output files exist
  expect_true(file.exists(file.path(outdir, imd_files()$metadata)))
  expect_true(file.exists(file.path(outdir, imd_files()$annotations)))
})

test_that("read_repertoires() works with <metadata>", {
  # Load example data shipped with your package
  md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")

  # This function presumably reads the metadata file
  # (If you have a 'read_metadata()' or 'load_metadata()' function.)
  metadata_df <- read_metadata(md_path)

  outdir <- tempdir()

  # Run the function
  imdata <- read_repertoires(
    path = "<metadata>",
    schema = c("cdr3_aa", "v_call"),
    metadata = metadata_df,
    output_folder = outdir
  )

  # Basic check: Did we get an object back?
  expect_true(!is.null(imdata))

  checkmate::expect_r6(imdata, classes = "ImmunData")

  # Check if the output files exist
  expect_true(file.exists(file.path(outdir, imd_files()$metadata)))
  expect_true(file.exists(file.path(outdir, imd_files()$annotations)))
})

test_that("read_repertoires() case 1: no barcode_col and no count_col", {
  # Provide test data that doesn't have barcodes or counts
  # e.g. a minimal TSV with just 'cdr3_aa' and 'v_call'
  small_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  # Create a fresh temp folder
  outdir <- file.path(tempdir(), "test-no-barcodes")
  dir.create(outdir, showWarnings = FALSE)

  # If you want to ensure no barcodes:
  # We'll skip 'cell_id_col' and 'count_col'
  imdata <- read_repertoires(
    path = small_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = outdir
  )

  checkmate::expect_r6(imdata, classes = "ImmunData")

  # Check that the function didn't crash and files were saved
  expect_true(file.exists(file.path(outdir, imd_files()$metadata)))
  expect_true(file.exists(file.path(outdir, imd_files()$annotations)))

  # You can do further checks on the resulting annotation columns, etc.
})

test_that("read_repertoires() errors when both barcode_col and count_col are set", {
  # Provide minimal data but pass both arguments to see if it triggers the expected error
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

test_that("read_repertoires() excludes specified columns", {
  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  exclude_cols <- c("sequence", "fwr1", "cdr1")

  imdata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"), # columns that do exist
    preprocess = list(
      exclude_columns = make_exclude_columns(cols = exclude_cols)
    ),
    output_folder = file.path(tempdir(), "test-exclude")
  )

  # If `annotations` is an R6 active binding (and not a function call), use:
  ann_cols <- colnames(imdata$annotations)

  # Check that the excluded columns are not present
  for (col in exclude_cols) {
    expect_false(
      col %in% ann_cols,
      info = paste("Column", col, "should have been excluded but is still present.")
    )
  }
})

test_that("read_repertoires() correctly renames columns (v_call -> v_gene)", {
  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  # We assume the file actually contains 'v_call'
  # We'll rename 'v_call' to 'v_gene'
  rename_map <- c("v_gene" = "v_call") # new_col = old_col

  imdata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_gene"), # We still rely on the old col name for grouping
    rename_columns = rename_map,
    output_folder = file.path(tempdir(), "test-rename")
  )

  # Access the annotation columns
  ann_cols <- colnames(imdata$annotations)

  expect_true(
    "v_gene" %in% ann_cols,
    info = "Renamed column 'v_call' -> 'v_gene' should appear in the annotation."
  )
  expect_false(
    "v_call" %in% ann_cols,
    info = "Original column 'v_call' should be removed after rename."
  )
})

test_that("read_repertoires() excludes columns AND renames simultaneously", {
  sample_file <- system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")

  # Suppose the data has columns "j_call" and we want to rename it to "j_gene"
  rename_map <- c("j_gene" = "j_call")
  exclude_cols <- c("cdr2", "fwr2") # must exist in sample_1k_2k.tsv for the test to pass

  imdata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call", "j_gene"),
    preprocess = list(
      exclude_columns = make_exclude_columns(cols = exclude_cols)
    ),
    rename_columns = rename_map,
    output_folder = file.path(tempdir(), "test-exclude-rename")
  )

  ann_cols <- colnames(imdata$annotations)

  # Check exclusion
  for (col in exclude_cols) {
    expect_false(
      col %in% ann_cols,
      info = paste("Column", col, "should have been excluded.")
    )
  }

  # Check rename
  expect_true(
    "j_gene" %in% ann_cols,
    info = "Renamed column 'j_call' -> 'j_gene' should appear."
  )
  expect_false(
    "j_call" %in% ann_cols,
    info = "Original column 'j_call' should be gone."
  )
})

test_that("read_repertoires() fails if missing columns in the receptor schema", {
  # Provide a sample input file known to have certain columns (like "cdr3_aa" and "v_call").
  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  # We intentionally add a column ("some_missing_col") that doesn't exist in the file
  bad_schema <- c("cdr3_aa", "v_call", "some_missing_col")

  expect_error(
    read_repertoires(
      path = sample_file,
      schema = bad_schema
    ),
    "Not all columns in the receptor schema present in the data"
  )
})
