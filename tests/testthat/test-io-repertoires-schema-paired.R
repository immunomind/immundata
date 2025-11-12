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
