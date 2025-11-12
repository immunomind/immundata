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
  expect_equal(igh_chains$junction_aa, "CARX") # 150 UMI
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
