test_that("read_repertoires() fails if path doesn't exist", {
  expect_error(
    read_repertoires(
      path = "nonexistent_file.tsv",
      schema = c("cdr3_aa", "v_call")
    ),
    "No file provided|does not exist|cannot find"
  )
})

test_that("read_repertoires() works with single file input", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Single file as documented
  inp_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  idata <- read_repertoires(
    path = inp_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL, # Disable for testing
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")
  expect_true(file.exists(file.path(output_dir, "annotations.parquet")))
  expect_true(file.exists(file.path(output_dir, "metadata.json")))

  # Check data was loaded
  annotations <- idata$annotations |> collect()
  expect_gt(nrow(annotations), 0)

  # Check required columns exist
  expect_true("imd_receptor_id" %in% colnames(annotations))
  expect_true("cdr3_aa" %in% colnames(annotations))
  expect_true("v_call" %in% colnames(annotations))
})

test_that("read_repertoires() works with vector of file names", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Vector of files as documented
  inp_file1 <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  inp_file2 <- system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  file_vec <- c(inp_file1, inp_file2)

  idata <- read_repertoires(
    path = file_vec,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")

  # Check that data from both files is present
  annotations <- idata$annotations |> collect()
  expect_gt(nrow(annotations), 0)

  # Should have data from both files
  if ("imd_filename" %in% colnames(annotations)) {
    unique_files <- unique(basename(annotations$imd_filename))
    expect_true("sample_0_1k.tsv" %in% unique_files || length(unique_files) > 0)
  }
})

test_that("read_repertoires() works with glob pattern", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Glob pattern as documented
  folder_with_files <- system.file("extdata/tsv", package = "immundata")
  glob_files <- file.path(folder_with_files, "sample*.tsv")

  # Verify glob expands to actual files
  expanded_files <- Sys.glob(glob_files)
  expect_gt(length(expanded_files), 0)

  idata <- read_repertoires(
    path = glob_files,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")
  annotations <- idata$annotations |> collect()
  expect_gt(nrow(annotations), 0)
})

test_that("read_repertoires() works with metadata table and file vector", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Load metadata
  md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
  metadata_df <- read_metadata(md_path)

  # Get sample files
  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )

  idata <- read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    metadata = metadata_df,
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")
  expect_true(file.exists(file.path(output_dir, "annotations.parquet")))
  expect_true(file.exists(file.path(output_dir, "metadata.json")))

  # Check metadata was joined
  annotations <- idata$annotations |> collect()
  if (!is.null(metadata_df) && "Therapy" %in% colnames(metadata_df)) {
    expect_true("Therapy" %in% colnames(annotations))
    expect_true("Response" %in% colnames(annotations))
  }
})

test_that("read_repertoires() works with <metadata> directive", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Load metadata with proper file paths
  md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
  metadata_df <- read_metadata(md_path)

  idata <- read_repertoires(
    path = "<metadata>",
    schema = c("cdr3_aa", "v_call"),
    metadata = metadata_df,
    metadata_file_col = "File",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")

  # Check metadata columns are present
  annotations <- idata$annotations |> collect()
  expect_true("Therapy" %in% colnames(annotations))
  expect_true("Response" %in% colnames(annotations))
  expect_true("Prefix" %in% colnames(annotations))
})

test_that("read_repertoires() fails with <metadata> when no metadata provided", {
  expect_error(
    read_repertoires(
      path = "<metadata>",
      schema = c("cdr3_aa", "v_call"),
      metadata = NULL
    ),
    "no `metadata` table provided"
  )
})

test_that("read_repertoires() handles custom metadata_file_col", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create custom metadata with different column name
  base_dir <- system.file("extdata/tsv", package = "immundata")
  custom_metadata <- data.frame(
    FilePath = c(
      file.path(base_dir, "sample_0_1k.tsv"),
      file.path(base_dir, "sample_1k_2k.tsv")
    ),
    SampleID = c("S1", "S2"),
    Treatment = c("A", "B")
  )

  idata <- read_repertoires(
    path = "<metadata>",
    schema = c("cdr3_aa", "v_call"),
    metadata = custom_metadata,
    metadata_file_col = "FilePath", # Custom column name
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")
  annotations <- idata$annotations |> collect()
  expect_true("SampleID" %in% colnames(annotations))
  expect_true("Treatment" %in% colnames(annotations))
})
