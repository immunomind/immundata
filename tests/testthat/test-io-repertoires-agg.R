# ============================================================================
# TEST 1: Single-chain data with duplicated receptors
# ============================================================================

test_that("agg_repertoires counts single-chain receptors correctly", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create synthetic single-chain data
  # Sample1: 3 cells, 2 unique receptors (cells 1&2 share receptor)
  # Sample2: 2 cells, 2 unique receptors
  test_data <- data.frame(
    cell_id = c("cell1", "cell2", "cell3", "cell4", "cell5"),
    sample_id = c("Sample1", "Sample1", "Sample1", "Sample2", "Sample2"),
    v_call = c("IGHV1", "IGHV1", "IGHV2", "IGHV3", "IGHV4"),
    j_call = c("IGHJ1", "IGHJ1", "IGHJ2", "IGHJ3", "IGHJ4"),
    junction_aa = c("CARW", "CARW", "CBRW", "CCRW", "CDRW"), # cell1 & cell2 identical
    locus = c("IGH", "IGH", "IGH", "IGH", "IGH"),
    umi_count = c(100, 150, 200, 250, 300)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  # Read and aggregate
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

  # Aggregate repertoires by sample_id
  idata_agg <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "single-chain duplicated receptors"
  )

  repertoires <- idata_agg$repertoires |> collect()
  annotations <- idata_agg$annotations |> collect()

  # Expected results:
  # Sample1: n_barcodes = 3, n_receptors = 2 (cells 1&2 share receptor)
  # Sample2: n_barcodes = 2, n_receptors = 2

  sample1_stats <- repertoires |> filter(sample_id == "Sample1")
  sample2_stats <- repertoires |> filter(sample_id == "Sample2")

  expect_equal(sample1_stats$n_barcodes, 3,
    info = "Sample1 should have 3 barcodes"
  )
  expect_equal(sample1_stats$n_receptors, 2,
    info = "Sample1 should have 2 unique receptors"
  )
  expect_equal(sample2_stats$n_barcodes, 2,
    info = "Sample2 should have 2 barcodes"
  )
  expect_equal(sample2_stats$n_receptors, 2,
    info = "Sample2 should have 2 unique receptors"
  )

  # Verify receptor counts within repertoires
  receptor_counts <- annotations |>
    group_by(sample_id, imd_receptor_id) |>
    summarise(
      total_count1 = first(imd_count),
      total_count2 = last(imd_count),
      .groups = "drop"
    )

  # The shared receptor in Sample1 should have count = 2
  sample1_counts <- receptor_counts |>
    filter(sample_id == "Sample1") |>
    arrange(desc(total_count1))

  expect_equal(sample1_counts$total_count1[1], 2,
    info = "Shared receptor should have count of 2"
  )

  sample1_counts2 <- receptor_counts |>
    filter(sample_id == "Sample1") |>
    arrange(desc(total_count2))

  expect_equal(sample1_counts$total_count2[1], 2,
    info = "Shared receptor should have count of 2"
  )
})

# ============================================================================
# TEST 2: Paired-chain data - potential double counting issue
# ============================================================================

test_that("agg_repertoires identifies double-counting in paired-chain data", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create synthetic paired-chain data
  # Each receptor appears TWICE (once per chain)
  test_data <- data.frame(
    cell_id = c("cell1", "cell1", "cell2", "cell2", "cell3", "cell3"),
    sample_id = c("Sample1", "Sample1", "Sample1", "Sample1", "Sample2", "Sample2"),
    v_call = c("IGHV1", "IGLV1", "IGHV2", "IGLV2", "IGHV3", "IGLV3"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ2", "IGLJ2", "IGHJ3", "IGLJ3"),
    junction_aa = c("CARW", "CASL", "CBRW", "CBSL", "CCRW", "CCSL"),
    locus = c("IGH", "IGL", "IGH", "IGL", "IGH", "IGL"),
    umi_count = c(100, 100, 150, 150, 200, 200) # Same count for both chains
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  # Read with paired-chain schema
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

  # Aggregate repertoires
  idata_agg <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "paired-chain double-counting check"
  )

  repertoires <- idata_agg$repertoires |> collect()
  annotations <- idata_agg$annotations |> collect()

  # Current behavior (with double-counting bug):
  # Sample1: n_barcodes will be 4 (2 cells × 2 chains) - WRONG!
  # Sample2: n_barcodes will be 2 (1 cell × 2 chains) - WRONG!
  # The correct values should be:
  # Sample1: n_barcodes = 2 (actual cells)
  # Sample2: n_barcodes = 1 (actual cell)

  sample1_stats <- repertoires |> filter(sample_id == "Sample1")
  sample2_stats <- repertoires |> filter(sample_id == "Sample2")

  # These tests document the CURRENT behavior (with double-counting)
  # They should FAIL when the bug is fixed
  current_behavior_sample1 <- sample1_stats$n_barcodes
  current_behavior_sample2 <- sample2_stats$n_barcodes

  # Test for n_receptors (this should be correct)
  expect_equal(sample1_stats$n_receptors, 2,
    info = "Sample1 should have 2 unique receptors"
  )
  expect_equal(sample2_stats$n_receptors, 1,
    info = "Sample2 should have 1 unique receptor"
  )

  expect_equal(sample1_stats$n_barcodes, 2)
  expect_equal(sample2_stats$n_barcodes, 1)
})

# ============================================================================
# TEST 3: Mixed scenario - paired-chain with shared receptors
# ============================================================================

test_that("agg_repertoires handles paired-chain data with shared receptors", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create paired-chain data where some cells share receptors
  test_data <- data.frame(
    cell_id = c("cell1", "cell1", "cell2", "cell2", "cell3", "cell3"),
    sample_id = c("Sample1", "Sample1", "Sample1", "Sample1", "Sample1", "Sample1"),
    v_call = c("IGHV1", "IGLV1", "IGHV1", "IGLV1", "IGHV2", "IGLV2"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ1", "IGLJ1", "IGHJ2", "IGLJ2"),
    junction_aa = c("CARW", "CASL", "CARW", "CASL", "CBRW", "CBSL"),
    locus = c("IGH", "IGL", "IGH", "IGL", "IGH", "IGL"),
    umi_count = c(100, 100, 150, 150, 200, 200)
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

  idata_agg <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "paired-chain shared receptors"
  )

  repertoires <- idata_agg$repertoires |> collect()
  annotations <- idata_agg$annotations |> collect()

  # Expected (correct) results:
  # - 3 actual cells
  # - 2 unique receptors (cells 1&2 share)
  # Current (incorrect due to double-counting):
  # - n_barcodes will show 6 (3 cells × 2 chains)

  expect_equal(repertoires$n_receptors, 2,
    info = "Should have 2 unique receptors"
  )
  # Check receptor counts
  receptor_counts <- annotations |>
    select(imd_receptor_id, imd_count) |>
    distinct()

  # The shared receptor should have count = 2 × 2 = 4 (double-counted)
  # Should be 2 (two cells sharing the receptor)
  shared_receptor_count <- max(receptor_counts$imd_count)
  expect_equal(shared_receptor_count, 2)
})

# ============================================================================
# TEST 4: Proportions calculation with double-counting
# ============================================================================

test_that("agg_repertoires proportions are affected by double-counting", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Simple paired-chain data for clear proportion calculation
  test_data <- data.frame(
    cell_id = c("cell1", "cell1", "cell2", "cell2"),
    sample_id = c("Sample1", "Sample1", "Sample1", "Sample1"),
    v_call = c("IGHV1", "IGLV1", "IGHV2", "IGLV2"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ2", "IGLJ2"),
    junction_aa = c("CARW", "CASL", "CBRW", "CBSL"),
    locus = c("IGH", "IGL", "IGH", "IGL"),
    umi_count = c(100, 100, 100, 100) # Equal counts
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

  idata_agg <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "paired-chain proportions"
  )
  annotations <- idata_agg$annotations |> collect()

  # Get unique proportions
  proportions <- annotations |>
    select(imd_receptor_id, imd_proportion) |>
    distinct()

  expect_equal(unique(proportions$imd_proportion), 0.5)
})

# ============================================================================
# TEST 5: Verify n_repertoires calculation (should be correct)
# ============================================================================

test_that("agg_repertoires n_repertoires calculation is correct for paired data", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create data where one receptor appears in multiple samples
  test_data <- data.frame(
    cell_id = c("cell1", "cell1", "cell2", "cell2", "cell3", "cell3"),
    sample_id = c("Sample1", "Sample1", "Sample2", "Sample2", "Sample3", "Sample3"),
    v_call = c("IGHV1", "IGLV1", "IGHV1", "IGLV1", "IGHV2", "IGLV2"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ1", "IGLJ1", "IGHJ2", "IGLJ2"),
    junction_aa = c("CARW", "CASL", "CARW", "CASL", "CBRW", "CBSL"),
    locus = c("IGH", "IGL", "IGH", "IGL", "IGH", "IGL"),
    umi_count = rep(100, 6)
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

  idata_agg <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "paired-chain n_repertoires"
  )
  annotations <- idata_agg$annotations |> collect()

  # Check n_repertoires for the shared receptor
  n_repertoires_vals <- annotations |>
    select(imd_receptor_id, n_repertoires) |>
    distinct()

  # First receptor appears in Sample1 and Sample2 (n_repertoires = 2)
  # Second receptor appears only in Sample3 (n_repertoires = 1)

  expect_true(2 %in% n_repertoires_vals$n_repertoires,
    info = "Shared receptor should appear in 2 repertoires"
  )
  expect_true(1 %in% n_repertoires_vals$n_repertoires,
    info = "Unique receptor should appear in 1 repertoire"
  )
})

test_that("agg_repertoires preserves second chain data (no NAs)", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  # Create paired-chain data
  # Cell1: IGH + IGL
  test_data <- data.frame(
    cell_id = c("cell1", "cell1"),
    sample_id = c("Sample1", "Sample1"),
    v_call = c("IGHV1", "IGLV1"),
    j_call = c("IGHJ1", "IGLJ1"),
    junction_aa = c("CARW", "CASL"),
    locus = c("IGH", "IGL"),
    umi_count = c(100, 100)
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
    output_folder = output_dir
  )

  # Run aggregation
  idata_agg <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "paired-chain no-NA preservation"
  )
  annotations <- idata_agg$annotations |> collect()

  # ----------------------------------------------------------------------
  # CHECK 1: Row Count
  # The input had 2 rows (1 cell x 2 chains).
  # The output MUST have 2 rows.
  # (The bug caused this to drop to 1 row).
  # ----------------------------------------------------------------------
  expect_equal(nrow(annotations), 2,
    info = "Annotation table should retain rows for both chains (IGH and IGL)"
  )

  # ----------------------------------------------------------------------
  # CHECK 2: Second Chain Data Presence
  # Filter for the Light Chain row and ensure V-call is present (not NA)
  # ----------------------------------------------------------------------
  igl_row <- annotations |> filter(locus == "IGL")

  expect_equal(nrow(igl_row), 1,
    info = "Should find exactly one row for the IGL chain"
  )

  expect_false(is.na(igl_row$v_call),
    info = "IGL row should have valid v_call data (not NA)"
  )
  expect_equal(igl_row$v_call, "IGLV1",
    info = "IGL v_call should match input data"
  )

  # ----------------------------------------------------------------------
  # CHECK 3: Statistics Mapping
  # Ensure the calculated stats (which are per-receptor) are mapped
  # correctly to BOTH chain rows.
  # ----------------------------------------------------------------------
  # Both IGH and IGL rows belong to the same receptor, so they should
  # both have the same imd_count and imd_proportion.

  igh_row <- annotations |> filter(locus == "IGH")

  expect_equal(igh_row$imd_count, igl_row$imd_count,
    info = "Both chains of the same receptor should share the same count stats"
  )
  expect_equal(igh_row$imd_repertoire_id, igl_row$imd_repertoire_id,
    info = "Both chains should belong to the same repertoire ID"
  )
})

test_that("agg_repertoires keeps repertoire IDs aligned with their schema", {
  output_dir <- create_test_output_dir("repertoire_mapping_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  set.seed(1)

  n_repertoires <- 128L
  n_barcodes <- 1000L
  n_rows <- n_repertoires * n_barcodes

  annotations <- tibble::tibble(
    sample_id = rep(
      sprintf("sample_%03d", seq_len(n_repertoires)),
      each = n_barcodes
    ),
    imd_receptor_id = rep(
      seq_len(n_repertoires),
      each = n_barcodes
    ),
    imd_barcode = seq_len(n_rows),
    imd_chain_id = seq_len(n_rows),
    imd_n_chains = 1L,
    cdr3_aa = rep(
      sprintf("CASS%03d", seq_len(n_repertoires)),
      each = n_barcodes
    )
  ) |>
    dplyr::slice_sample(prop = 1) |>
    duckplyr::as_duckdb_tibble()

  idata <- ImmunData$new(
    schema = "cdr3_aa",
    annotations = annotations
  )

  aggregated <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "repertoire ID and schema mapping"
  )

  expected_mapping <- tibble::tibble(
    imd_repertoire_id = seq_len(n_repertoires),
    sample_id = sprintf("sample_%03d", seq_len(n_repertoires))
  )

  actual_mapping <- aggregated$repertoires |>
    dplyr::select(imd_repertoire_id, sample_id) |>
    dplyr::arrange(imd_repertoire_id) |>
    dplyr::collect()

  expect_equal(actual_mapping, expected_mapping)

  write_immundata(aggregated, output_folder = output_dir)
  loaded <- read_immundata(output_dir, verbose = FALSE)

  expect_agg_repertoires_integrity(
    loaded,
    context = "repertoire ID and schema mapping after snapshot roundtrip",
    schema = "sample_id"
  )

  loaded_mapping <- loaded$repertoires |>
    dplyr::select(imd_repertoire_id, sample_id) |>
    dplyr::arrange(imd_repertoire_id) |>
    dplyr::collect()

  expect_equal(loaded_mapping, expected_mapping)
})

test_that("agg_repertoires orders IDs by a composite schema", {
  annotations <- tibble::tibble(
    sample_id = c("sample_b", "sample_a", "sample_b", "sample_a"),
    timepoint = c("day_2", "day_2", "day_1", "day_1"),
    imd_receptor_id = seq_len(4L),
    imd_barcode = seq_len(4L),
    imd_chain_id = seq_len(4L),
    imd_n_chains = 1L,
    cdr3_aa = paste0("CASS", seq_len(4L))
  ) |>
    duckplyr::as_duckdb_tibble()

  idata <- ImmunData$new(
    schema = "cdr3_aa",
    annotations = annotations
  )

  aggregated <- agg_repertoires_with_integrity(
    idata,
    schema = c("sample_id", "timepoint"),
    context = "composite repertoire schema mapping"
  )

  actual_mapping <- aggregated$repertoires |>
    dplyr::select(imd_repertoire_id, sample_id, timepoint) |>
    dplyr::arrange(imd_repertoire_id) |>
    dplyr::collect()

  expected_mapping <- tibble::tribble(
    ~imd_repertoire_id, ~sample_id, ~timepoint,
    1L, "sample_a", "day_1",
    2L, "sample_a", "day_2",
    3L, "sample_b", "day_1",
    4L, "sample_b", "day_2"
  )

  expect_equal(actual_mapping, expected_mapping)
})
