# ============================================================================
# USE CASE 1: SINGLE CHAIN
# ============================================================================

test_that("Single chain: correct barcode and receptor counts", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create toy data: 5 cells, all with IGH
  test_data <- make_single_chain_shared_receptor_test_data()

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = "IGH"
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()

  # Expected counts:
  # - 5 barcodes (all cells have IGH)
  # - 4 unique receptors (cell1 and cell3 have identical receptors)

  expect_equal(n_distinct(annotations$imd_barcode), 5)
  expect_equal(n_distinct(annotations$imd_receptor_id), 4)

  # Verify receptor sharing
  receptor_counts <- annotations |>
    group_by(imd_receptor_id) |>
    summarise(n_cells = n_distinct(imd_barcode), .groups = "drop")

  expect_true(any(receptor_counts$n_cells == 2))
})

test_that("Single chain with mixed loci: filters correctly", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create toy data with mixed loci
  test_data <- data.frame(
    cell_id = c("cell1", "cell2", "cell3", "cell4", "cell5"),
    v_call = c("IGHV1", "IGLV1", "IGHV2", "IGKV1", "IGHV3"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ2", "IGKJ1", "IGHJ3"),
    junction_aa = c("CARW", "CASL", "CBRW", "CASK", "CCRW"),
    locus = c("IGH", "IGL", "IGH", "IGK", "IGH"),
    umi_count = c(100, 200, 150, 300, 250)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  # Request only IGH chains
  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = "IGH"
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()

  # Expected: Only cells 1, 3, and 5 (those with IGH)
  expect_equal(n_distinct(annotations$imd_barcode), 3,
    info = "Should have only 3 cells with IGH"
  )
  expect_setequal(annotations$imd_barcode, c("cell1", "cell3", "cell5"))
})

# ============================================================================
# USE CASE 2: STRICT PAIRING
# ============================================================================

test_that("Strict pairing: correct counts for perfect pairs", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create toy data with perfect IGH+IGL pairs
  test_data <- data.frame(
    cell_id = c("cell1", "cell1", "cell2", "cell2", "cell3", "cell3"),
    v_call = c("IGHV1", "IGLV1", "IGHV2", "IGLV2", "IGHV1", "IGLV1"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ2", "IGLJ2", "IGHJ1", "IGLJ1"),
    junction_aa = c("CARW", "CASL", "CBRW", "CBSL", "CARW", "CASL"),
    locus = c("IGH", "IGL", "IGH", "IGL", "IGH", "IGL"),
    umi_count = c(100, 80, 120, 90, 110, 85)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = c("IGH", "IGL")
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()

  # Expected:
  # - 3 barcodes (all have complete pairs)
  # - 2 unique receptors (cell1 and cell3 share the same receptor)

  expect_equal(n_distinct(annotations$imd_barcode), 3)
  expect_equal(n_distinct(annotations$imd_receptor_id), 2)
})

test_that("Strict pairing: excludes incomplete pairs", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create toy data with some incomplete pairs
  test_data <- data.frame(
    cell_id = c("cell1", "cell1", "cell2", "cell3", "cell4", "cell4"),
    v_call = c("IGHV1", "IGLV1", "IGHV2", "IGLV3", "IGHV4", "IGKV4"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ2", "IGLJ3", "IGHJ4", "IGKJ4"),
    junction_aa = c("CARW", "CASL", "CBRW", "CCSL", "CDRW", "CDSK"),
    locus = c("IGH", "IGL", "IGH", "IGL", "IGH", "IGK"),
    umi_count = c(100, 80, 120, 90, 110, 85)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  # Request strict IGH+IGL pairing
  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = c("IGH", "IGL")
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()

  # Expected: Only cell1 has both IGH and IGL
  # cell2 has only IGH, cell3 has only IGL, cell4 has IGH+IGK (wrong light chain)

  expect_equal(n_distinct(annotations$imd_barcode), 1)
  expect_equal(unique(annotations$imd_barcode), "cell1")
})

test_that("Strict pairing: handles duplicate chains correctly", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create data with multiple chains of same locus per cell
  test_data <- make_duplicate_chain_test_data()

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = c("IGH", "IGL")
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()

  # Should select highest UMI for each locus
  expect_equal(nrow(annotations), 2)

  igh_chain <- annotations |> filter(locus == "IGH")
  igl_chain <- annotations |> filter(locus == "IGL")

  expect_equal(igh_chain$v_call, "IGHV2")
  expect_equal(igl_chain$v_call, "IGLV1")
})

# ============================================================================
# USE CASE 3: RELAXED PAIRING WITH ARTIFACT EXCLUSION
# ============================================================================

test_that("Relaxed pairing: correct counts with artifact exclusion", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create toy data with various scenarios
  test_data <- make_relaxed_pairing_test_data()

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = c("IGH", "IGL|IGK") # Relaxed pairing
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()

  # Expected results:
  # - normal_igl: IGH + IGL ✓ (valid)
  # - normal_igk: IGH + IGK ✓ (valid)
  # - artifact: EXCLUDED (has both IGL and IGK)
  # - heavy_only: EXCLUDED (missing light chain)
  # - light_only: EXCLUDED (missing heavy chain)
  # - two_lights: EXCLUDED (missing heavy chain)

  expect_equal(n_distinct(annotations$imd_barcode), 2)
  expect_setequal(unique(annotations$imd_barcode), c("normal_igl", "normal_igk"))
  expect_equal(n_distinct(annotations$imd_receptor_id), 2)
})

# ============================================================================
# USE CASE 3.5: BULK DATA (no barcodes, with counts)
# ============================================================================

test_that("Bulk data: correct receptor counts without barcodes", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create bulk data (no cell IDs, but with clone counts)
  test_data <- data.frame(
    v_call = c("IGHV1", "IGHV1", "IGHV2", "IGHV3"),
    j_call = c("IGHJ1", "IGHJ1", "IGHJ2", "IGHJ3"),
    junction_aa = c("CARW", "CARW", "CBRW", "CCRW"),
    locus = c("IGH", "IGH", "IGH", "IGH"),
    clone_count = c(100, 50, 200, 75) # Frequency counts
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = "IGH"
    ),
    count_col = "clone_count", # Bulk mode with counts
    locus_col = "locus",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  annotations <- idata$annotations |> collect()

  # Expected:
  # - 4 synthetic barcodes (one per row)
  # - 3 unique receptors (rows 1 and 2 have identical receptors)
  # - Chain counts should match clone_count values

  expect_equal(n_distinct(annotations$imd_barcode), 4)
  expect_equal(n_distinct(annotations$imd_receptor_id), 3)
  expect_equal(annotations$imd_n_chains, test_data$clone_count)
})

test_that("agg_receptors rejects negative bulk counts", {
  dataset <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      v_call = c("IGHV1", "IGHV2"),
      j_call = c("IGHJ1", "IGHJ2"),
      junction_aa = c("CARW", "CBRW"),
      clone_count = c(10, -1)
    )
  )

  expect_error(
    agg_receptors(
      dataset = dataset,
      schema = c("v_call", "j_call", "junction_aa"),
      count_col = "clone_count"
    ),
    "non-negative|negative"
  )
})

# ============================================================================
# COMPARISON TESTS
# ============================================================================

test_that("Comparison: relaxed vs strict pairing counts", {
  output_dir_strict <- create_test_output_dir()
  output_dir_relaxed <- create_test_output_dir()
  on.exit({
    cleanup_output_dir(output_dir_strict)
    cleanup_output_dir(output_dir_relaxed)
  })

  test_data <- make_relaxed_pairing_test_data()

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  # Strict IGH + IGL
  idata_strict <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = c("IGH", "IGL")
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir_strict,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  # Relaxed IGH + (IGL|IGK)
  idata_relaxed <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = c("IGH", "IGL|IGK")
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir_relaxed,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  strict_cells <- idata_strict$annotations |>
    collect() |>
    pull(imd_barcode) |>
    unique()
  relaxed_cells <- idata_relaxed$annotations |>
    collect() |>
    pull(imd_barcode) |>
    unique()

  # Strict should include: normal_igl, artifact (picking IGL)
  # Relaxed should include: normal_igl, normal_igk (excluding artifact)

  expect_true("artifact" %in% strict_cells,
    info = "Strict pairing includes artifact cell"
  )
  expect_false("artifact" %in% relaxed_cells,
    info = "Relaxed pairing excludes artifact cell"
  )
  expect_equal(length(relaxed_cells), 2,
    info = "Relaxed should have 2 valid cells"
  )
})
