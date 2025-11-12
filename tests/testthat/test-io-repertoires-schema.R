test_that("Case 1: read_repertoires() handles table case correctly", {
  small_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  # Create a fresh temp folder
  outdir <- file.path(tempdir(), "test-no-barcodes")
  dir.create(outdir, showWarnings = FALSE)

  schema_features <- c("cdr3_aa", "v_call")

  idata <- read_repertoires(
    path = small_file,
    schema = schema_features,
    output_folder = outdir
  )

  annotations <- idata$annotations |> collect()
  receptors <- idata$receptors |> collect()

  expect_false(nrow(annotations) == 0)

  expect_setequal(colnames(idata$receptors), c(schema_features, imd_schema("receptor")))

  expect_equal(receptors |> select(-imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))
  expect_equal(receptors |> select(imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))

  checkmate::expect_r6(idata, classes = "ImmunData")

  expect_true(file.exists(file.path(outdir, imd_files()$metadata)))
  expect_true(file.exists(file.path(outdir, imd_files()$annotations)))
})

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

test_that("Case 3.1: read_repertoires() handles single chain correctly", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  schema_features <- c("v_call", "j_call", "junction_aa")

  sample_file <- test_ig_data()

  idata <- read_repertoires(
    path = sample_file,
    schema = make_receptor_schema(
      features = schema_features,
      chains = "IGH"
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  # Tests
  expect_s3_class(idata, "ImmunData")

  annotations <- idata$annotations |> collect()
  receptors <- idata$receptors |> collect()

  expect_false(nrow(annotations) == 0)

  expect_equal(receptors |> select(-imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))
  expect_equal(receptors |> select(imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))

  expect_equal(unique(annotations$locus), "IGH")

  # Verify all IGH-containing cells from original data are represented
  original_data <- readr::read_tsv(sample_file, show_col_types = FALSE)
  cells_with_igh <- original_data |>
    filter(locus == "IGH") |>
    pull(cell_id) |>
    unique()

  cells_in_result <- unique(annotations$imd_barcode)
  expect_setequal(cells_in_result, cells_with_igh)
})

test_that("Case 3.2a: read_repertoires handles strict pairing correctly", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  schema_features <- c("v_call", "j_call", "junction_aa")

  sample_file <- test_ig_data()

  idata <- read_repertoires(
    path = sample_file,
    schema = make_receptor_schema(
      features = schema_features,
      chains = c("IGH", "IGK")
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "duplicate_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()
  receptors <- idata$receptors |> collect()

  # Sanity check
  expect_false(nrow(annotations) == 0)

  expect_setequal(colnames(idata$receptors), c(do.call(paste0, expand.grid(c(schema_features, "locus"), c(".x", ".y"))), imd_schema("receptor")))

  expect_equal(receptors |> select(-imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))
  expect_equal(receptors |> select(imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))

  chains_stats <- annotations |>
    summarise(n_chains = n(), .by = imd_barcode) |>
    summarise(n = n(), .by = n_chains)

  # We should see the receptors with two chains only, nothing more, nothing less
  expect_setequal(2, chains_stats$n_chains)

  # Each cell should have both IGH and IGK
  cell_loci <- annotations |>
    group_by(imd_barcode) |>
    summarise(
      loci = list(sort(unique(locus))),
      n_loci = n_distinct(locus),
      .groups = "drop"
    )

  # All cells should have exactly 2 loci
  expect_true(all(cell_loci$n_loci == 2))

  # All cells should have both IGH and IGK
  res <- cell_loci |>
    distinct(loci) |>
    pull(loci)
  expect_equal(res[[1]], c("IGH", "IGK"))
})

test_that("Case 3.2b: read_repertoires() handles relaxed pairing and excludes artifacts", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Test relaxed IGH + (IGK|IGL) pairing
  schema_features <- c("v_call", "j_call", "junction_aa")

  sample_file <- test_ig_data()

  idata <- read_repertoires(
    path = sample_file,
    schema = make_receptor_schema(
      features = schema_features,
      chains = c("IGH", "IGK|IGL") # Relaxed pairing syntax
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "duplicate_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  expect_setequal(colnames(idata$receptors), c(do.call(paste0, expand.grid(c(schema_features, "locus"), c(".x", ".y"))), imd_schema("receptor")))

  # Sanity check
  annotations <- idata$annotations |> collect()
  receptors <- idata$receptors |> collect()

  expect_false(nrow(annotations) == 0)

  expect_equal(receptors |> select(-imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))
  expect_equal(receptors |> select(imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))

  chains_stats <- annotations |>
    summarise(n_chains = n(), .by = imd_barcode) |>
    summarise(n = n(), .by = n_chains)

  # We should see the receptors with two chains only, nothing more, nothing less
  expect_setequal(2, chains_stats$n_chains)

  # Each cell should have both IGH and IGK
  cell_loci <- annotations |>
    group_by(imd_barcode) |>
    summarise(
      loci = list(sort(unique(locus))),
      n_loci = n_distinct(locus),
      .groups = "drop"
    )

  # All cells should have exactly 2 loci
  expect_true(all(cell_loci$n_loci == 2))

  # All cells should have both IGH and IGK
  res <- cell_loci |>
    distinct(loci) |>
    pull(loci)
  expect_setequal(res, list(c("IGH", "IGK"), c("IGH", "IGL")))

  # Identify artifact cells from original data (those with IGH + IGK + IGL)
  original_data <- readr::read_tsv(sample_file, show_col_types = FALSE)
  artifact_cells <- original_data |>
    group_by(cell_id) |>
    summarise(
      has_igh = "IGH" %in% locus,
      has_igk = "IGK" %in% locus,
      has_igl = "IGL" %in% locus,
      .groups = "drop"
    ) |>
    filter(!((has_igh & has_igk & !has_igl) | (has_igh & !has_igk & has_igl))) |>
    pull(cell_id)

  cells_in_result <- unique(annotations$imd_barcode)

  # Artifact cells should NOT be in the result
  expect_false(any(artifact_cells %in% cells_in_result))
})

test_that("read_repertoires handles duplicate chain entries correctly", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- tibble(
    cell_id = c("cell1", "cell1", "cell1", "cell1"),
    v_call = c("IGHV1", "IGHV1", "IGLV1", "IGLV2"),
    j_call = c("IGHJ1", "IGHJ1", "IGLJ1", "IGLJ2"),
    junction_aa = c("CARW", "CARX", "CASW", "CATW"),
    locus = c("IGH", "IGH", "IGL", "IGL"), # Two IGH and two IGL chains
    umi_count = c(100, 150, 80, 60) # Different UMI counts
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  schema_features <- c("v_call", "j_call", "junction_aa")

  idata_strict <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(features = schema_features, chains = c("IGH", "IGL")),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata_strict$annotations |> collect()

  # Should select highest UMI count for each locus
  igh_chains <- annotations |> filter(locus == "IGH")
  igl_chains <- annotations |> filter(locus == "IGL")

  # Should have selected the chains with highest UMI
  expect_equal(igh_chains$junction_aa, "CARX") # 150 UMI
  expect_equal(igl_chains$junction_aa, "CASW") # 80 UMI
})

test_that("read_repertoires handles same max UMI count per barcode", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- tibble(
    cell_id = c("cell1", "cell1", "cell1", "cell1"),
    v_call = c("IGHV1", "IGHV1", "IGLV1", "IGLV2"),
    j_call = c("IGHJ1", "IGHJ1", "IGLJ1", "IGLJ2"),
    junction_aa = c("CARW", "CARX", "CASW", "CATW"),
    locus = c("IGH", "IGH", "IGL", "IGL"), # Two IGH and two IGL chains
    umi_count = c(100, 100, 80, 80) # Different UMI counts
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  schema_features <- c("v_call", "j_call", "junction_aa")

  idata_strict <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(features = schema_features, chains = c("IGH", "IGL")),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata_strict$annotations |> collect()

  # Should select highest UMI count for each locus
  igh_chains <- annotations |> filter(locus == "IGH")
  igl_chains <- annotations |> filter(locus == "IGL")

  # Should have selected the chains with highest UMI
  expect_equal(igh_chains$junction_aa, "CARW") # first one
  expect_equal(igl_chains$junction_aa, "CASW") # first one as well
})
