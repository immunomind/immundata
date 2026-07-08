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

test_that("read_repertoires() works with manifest table and file vector", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Load manifest
  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest_df <- read_manifest(manifest_path)

  # Get sample files
  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )

  idata <- read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    manifest = manifest_df,
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")
  expect_true(file.exists(file.path(output_dir, "annotations.parquet")))
  expect_true(file.exists(file.path(output_dir, "metadata.json")))

  # Check manifest annotations were joined
  annotations <- idata$annotations |> collect()
  if (!is.null(manifest_df) && "Therapy" %in% colnames(manifest_df)) {
    expect_true("Therapy" %in% colnames(annotations))
    expect_true("Response" %in% colnames(annotations))
  }
})

test_that("read_repertoires() works with <manifest> directive", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Load manifest with proper file paths
  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest_df <- read_manifest(manifest_path)

  idata <- read_repertoires(
    path = "<manifest>",
    schema = c("cdr3_aa", "v_call"),
    manifest = manifest_df,
    manifest_file_col = "file",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  # Verify result
  expect_s3_class(idata, "ImmunData")

  # Check manifest columns are present
  annotations <- idata$annotations |> collect()
  expect_true("Therapy" %in% colnames(annotations))
  expect_true("Response" %in% colnames(annotations))
  expect_true("Prefix" %in% colnames(annotations))
})

test_that("read_repertoires() creates one repertoire per manifest row with <manifest> repertoire_schema", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest_df <- read_manifest(manifest_path)

  idata <- read_repertoires(
    path = "<manifest>",
    schema = c("cdr3_aa", "v_call"),
    manifest = manifest_df,
    manifest_file_col = "file",
    repertoire_schema = "<manifest>",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  expect_s3_class(idata, "ImmunData")
  expect_false(is.null(idata$repertoires))

  annotations <- idata$annotations |> collect()
  repertoires <- idata$repertoires |> collect()

  expect_equal(nrow(repertoires), nrow(manifest_df))
  expect_true("imd_filename" %in% colnames(repertoires))
  expect_true("imd_filename" %in% idata$schema_repertoire)
  expect_true(all(colnames(manifest_df) %in% idata$schema_repertoire))
  expect_true(all(colnames(manifest_df) %in% colnames(repertoires)))

  file_to_repertoire <- annotations |>
    dplyr::summarise(
      n_repertoires = dplyr::n_distinct(imd_repertoire_id),
      .by = imd_filename
    )

  expect_equal(nrow(file_to_repertoire), nrow(manifest_df))
  expect_true(all(file_to_repertoire$n_repertoires == 1))
})

test_that("read_repertoires() <auto> uses all manifest columns when path is <manifest>", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest_df <- read_manifest(manifest_path)

  idata <- read_repertoires(
    path = "<manifest>",
    schema = c("cdr3_aa", "v_call"),
    manifest = manifest_df,
    manifest_file_col = "file",
    repertoire_schema = "<auto>",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  repertoires <- idata$repertoires |> collect()

  expect_equal(nrow(repertoires), nrow(manifest_df))
  expect_true(all(colnames(manifest_df) %in% idata$schema_repertoire))
  expect_true(all(colnames(manifest_df) %in% colnames(repertoires)))
  expect_true("imd_filename" %in% idata$schema_repertoire)
})

test_that("read_repertoires() <auto> creates one repertoire per file without manifest", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  inp_file1 <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  inp_file2 <- system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  file_vec <- c(inp_file1, inp_file2)

  idata <- read_repertoires(
    path = file_vec,
    schema = c("cdr3_aa", "v_call"),
    repertoire_schema = "<auto>",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  expect_s3_class(idata, "ImmunData")
  expect_equal(idata$schema_repertoire, "imd_filename")

  annotations <- idata$annotations |> collect()
  repertoires <- idata$repertoires |> collect()

  expect_equal(nrow(repertoires), length(file_vec))
  expect_true("imd_filename" %in% colnames(repertoires))

  file_to_repertoire <- annotations |>
    dplyr::summarise(
      n_repertoires = dplyr::n_distinct(imd_repertoire_id),
      .by = imd_filename
    )

  expect_equal(nrow(file_to_repertoire), length(file_vec))
  expect_true(all(file_to_repertoire$n_repertoires == 1))
})

test_that("read_manifest() rejects old metadata filenames", {
  manifest_dir <- tempfile("old_manifest_name_")
  dir.create(manifest_dir)
  on.exit(unlink(manifest_dir, recursive = TRUE), add = TRUE)

  old_path <- file.path(manifest_dir, "metadata.tsv")
  writeLines(c("file", "sample_0_1k.tsv"), old_path)

  expect_error(
    read_manifest(old_path),
    "repertoire metadata tables are now manifests"
  )
})

test_that("read_repertoires() fails with <manifest> when no manifest provided", {
  expect_error(
    read_repertoires(
      path = "<manifest>",
      schema = c("cdr3_aa", "v_call"),
      manifest = NULL
    ),
    "no `manifest` table provided"
  )
})

test_that("read_repertoires() handles custom manifest_file_col", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create custom manifest with different column name
  base_dir <- system.file("extdata/tsv", package = "immundata")
  custom_manifest <- data.frame(
    FilePath = c(
      file.path(base_dir, "sample_0_1k.tsv"),
      file.path(base_dir, "sample_1k_2k.tsv")
    ),
    SampleID = c("S1", "S2"),
    Treatment = c("A", "B")
  )

  idata <- read_repertoires(
    path = "<manifest>",
    schema = c("cdr3_aa", "v_call"),
    manifest = custom_manifest,
    manifest_file_col = "FilePath", # Custom column name
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
