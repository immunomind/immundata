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
    umi_col = "duplicate_count",
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

  # Verify all valid IGH-containing cells from original data are represented
  cells_in_result <- unique(annotations$imd_barcode)
  original_data <- readr::read_tsv(sample_file, show_col_types = FALSE)

  cells_with_igh <- original_data |>
    group_by(cell_id) |>
    summarise(
      has_igh = "IGH" %in% locus,
      has_igk = "IGK" %in% locus,
      has_igl = "IGL" %in% locus,
      .groups = "drop"
    ) |>
    filter(has_igh) |>
    pull(cell_id) |>
    unique()

  expect_setequal(cells_in_result, cells_with_igh)
})

test_that("read_repertoires handles duplicate single-chain entries correctly", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- make_duplicate_chain_test_data()

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  schema_features <- c("v_call", "j_call", "junction_aa")

  idata_strict <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(features = schema_features, chains = c("IGH")),
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

  # Should have selected the chains with highest UMI
  expect_equal(igh_chains$junction_aa, "CBRW") # 150 UMI
})

test_that("read_repertoires handles tied max UMI counts for a single chain", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- make_duplicate_chain_test_data(tied = TRUE)

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  schema_features <- c("v_call", "j_call", "junction_aa")

  idata_strict <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(features = schema_features, chains = c("IGH")),
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

  # Should have selected the chains with highest UMI
  expect_equal(igh_chains$junction_aa, "CARW") # first one
})
