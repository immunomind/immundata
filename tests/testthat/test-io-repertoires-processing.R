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

  ann_cols <- colnames(imdata$annotations)

  for (col in exclude_cols) {
    expect_false(
      col %in% ann_cols,
      info = paste("Column", col, "should have been excluded but is still present.")
    )
  }
})

test_that("read_repertoires() correctly renames columns (v_call -> v_gene)", {
  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  rename_map <- c("v_gene" = "v_call")

  imdata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_gene"),
    rename_columns = rename_map,
    output_folder = file.path(tempdir(), "test-rename")
  )

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

  for (col in exclude_cols) {
    expect_false(
      col %in% ann_cols,
      info = paste("Column", col, "should have been excluded.")
    )
  }

  expect_true(
    "j_gene" %in% ann_cols,
    info = "Renamed column 'j_call' -> 'j_gene' should appear."
  )
  expect_false(
    "j_call" %in% ann_cols,
    info = "Original column 'j_call' should be gone."
  )
})

test_that("read_repertoires() removes non-productive", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  imdata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir
  )
  imdata_rows <- imdata |>
    count() |>
    pull()

  df <- readr::read_tsv(sample_file)
  n_prod <- sum(df$productive)

  expect_equal(
    imdata_rows, n_prod
  )
})

test_that("read_repertoires() correctly reads non-productive", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  imdata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    preprocess = NULL,
    output_folder = output_dir
  )
  imdata_rows <- imdata |>
    count() |>
    pull()

  df <- readr::read_tsv(sample_file)
  n_all <- df |>
    count() |>
    pull()

  expect_equal(
    imdata_rows,
    n_all
  )
})

test_that("read_repertoires() with repertoire_schema creates repertoires", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest_df <- read_manifest(manifest_path)

  idata <- read_repertoires(
    path = "<manifest>",
    schema = c("cdr3_aa", "v_call"),
    manifest = manifest_df,
    repertoire_schema = "Therapy",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  expect_true(!is.null(idata$repertoires))

  if (!is.null(idata$repertoires)) {
    repertoires <- idata$repertoires |> collect()
    expect_gt(nrow(repertoires), 0)
    expect_true("Therapy" %in% colnames(repertoires))
  }
})
