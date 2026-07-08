test_that("downsample_immundata downsamples single-cell repertoires and is deterministic with seed", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    cell_id = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"),
    sample_id = c("S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2"),
    v_call = c("IGHV1", "IGHV2", "IGHV3", "IGHV4", "IGHV1", "IGHV2", "IGHV3", "IGHV4"),
    j_call = c("IGHJ1", "IGHJ2", "IGHJ3", "IGHJ4", "IGHJ1", "IGHJ2", "IGHJ3", "IGHJ4"),
    junction_aa = c("CARA", "CARB", "CARC", "CARD", "CARE", "CARF", "CARG", "CARH"),
    locus = "IGH",
    umi_count = c(10, 11, 12, 13, 14, 15, 16, 17)
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
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "downsample single-cell deterministic"
  )

  ds1 <- downsample_immundata(idata, n = 2, seed = 100)
  ds2 <- downsample_immundata(idata, n = 2, seed = 100)

  expect_identical(imd_get_provenance(ds1), imd_get_provenance(idata))

  reps <- ds1$repertoires
  expect_true(all(reps$n_barcodes == 2))

  sampled1 <- ds1$annotations |>
    select(sample_id, imd_barcode) |>
    distinct() |>
    arrange(sample_id, imd_barcode) |>
    collect()

  sampled_counts <- sampled1 |>
    summarise(.by = sample_id, n_barcodes = n()) |>
    arrange(sample_id)

  sampled2 <- ds2$annotations |>
    select(sample_id, imd_barcode) |>
    distinct() |>
    arrange(sample_id, imd_barcode) |>
    collect()

  expect_equal(sampled_counts$n_barcodes, c(2L, 2L))
  expect_true(all(sampled1$imd_barcode %in% test_data$cell_id))
  expect_equal(sampled1, sampled2)
})

test_that("downsample_immundata downsampled bulk repertoires by count", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    sample_id = c("S1", "S1", "S2", "S2"),
    v_call = c("TRBV1", "TRBV2", "TRBV3", "TRBV4"),
    j_call = c("TRBJ1", "TRBJ2", "TRBJ1", "TRBJ2"),
    junction_aa = c("AAAA", "BBBB", "CCCC", "DDDD"),
    clone_count = c(8, 7, 6, 9)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = c("v_call", "j_call", "junction_aa"),
    count_col = "clone_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "downsample bulk count mode"
  )

  ds <- downsample_immundata(idata, n = 5, seed = 42)
  reps <- ds$repertoires
  ann <- ds$annotations |>
    select(sample_id, v_call, j_call, junction_aa, imd_n_chains, imd_count, imd_proportion) |>
    arrange(sample_id, v_call) |>
    collect()

  count_totals <- ann |>
    summarise(.by = sample_id, n_barcodes = sum(imd_n_chains)) |>
    arrange(sample_id)

  original_counts <- test_data |>
    rename(imd_n_chains_before = clone_count)

  ann_with_original <- ann |>
    left_join(original_counts, by = c("sample_id", "v_call", "j_call", "junction_aa"))

  expect_equal(sort(reps$n_barcodes), c(5, 5))
  expect_equal(count_totals$n_barcodes, c(5L, 5L))
  expect_true(all(ann$imd_count == ann$imd_n_chains))
  expect_true(all(ann$imd_proportion > 0))
  expect_true(all(ann_with_original$imd_n_chains <= ann_with_original$imd_n_chains_before))
})

test_that("downsample_immundata keeps paired chains intact", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    cell_id = c(
      "a1", "a1", "a2", "a2", "a3", "a3",
      "b1", "b1", "b2", "b2", "b3", "b3"
    ),
    sample_id = c(
      "S1", "S1", "S1", "S1", "S1", "S1",
      "S2", "S2", "S2", "S2", "S2", "S2"
    ),
    v_call = c("IGHV1", "IGLV1", "IGHV2", "IGLV2", "IGHV3", "IGLV3", "IGHV1", "IGLV1", "IGHV2", "IGLV2", "IGHV3", "IGLV3"),
    j_call = c("IGHJ1", "IGLJ1", "IGHJ2", "IGLJ2", "IGHJ3", "IGLJ3", "IGHJ1", "IGLJ1", "IGHJ2", "IGLJ2", "IGHJ3", "IGLJ3"),
    junction_aa = c("CA1", "CL1", "CA2", "CL2", "CA3", "CL3", "CB1", "CK1", "CB2", "CK2", "CB3", "CK3"),
    locus = c("IGH", "IGL", "IGH", "IGL", "IGH", "IGL", "IGH", "IGL", "IGH", "IGL", "IGH", "IGL"),
    umi_count = rep(100, 12)
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
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "downsample paired-chain integrity"
  )

  ds <- downsample_immundata(idata, n = 2, seed = 7)
  reps <- ds$repertoires
  expect_true(all(reps$n_barcodes == 2))

  chain_stats <- ds$annotations |>
    collect() |>
    summarise(
      .by = c(sample_id, imd_barcode),
      n_loci = n_distinct(locus),
      n_rows = n()
    )

  barcode_counts <- chain_stats |>
    summarise(.by = sample_id, n_barcodes = n()) |>
    arrange(sample_id)

  expect_equal(barcode_counts$n_barcodes, c(2L, 2L))
  expect_true(all(chain_stats$n_loci == 2))
  expect_true(all(chain_stats$n_rows == 2))
})

test_that("downsample_immundata works on IG test data with proportion n", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  idata <- read_repertoires(
    path = test_ig_data(),
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = c("IGH", "IGK|IGL")
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "duplicate_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  ) |>
    mutate_immundata(cohort = "all")
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "cohort",
    context = "downsample IG proportion"
  )

  n_before <- idata$repertoires |>
    pull(n_barcodes)

  ds <- downsample_immundata(idata, n = 0.5, seed = 123)
  n_after <- ds$repertoires |>
    pull(n_barcodes)

  expect_equal(n_after, floor(n_before * 0.5))

  annotation_units <- ds$annotations |>
    select(cohort, imd_barcode) |>
    distinct() |>
    collect() |>
    summarise(.by = cohort, n_barcodes = n())

  expect_equal(annotation_units$n_barcodes, n_after)

  chain_stats <- ds$annotations |>
    collect() |>
    summarise(
      .by = imd_barcode,
      n_loci = n_distinct(locus)
    )
  expect_true(all(chain_stats$n_loci == 2))
})

test_that("downsample_immundata rejects ambiguous n = 1", {
  idata <- get_test_idata_tsv_no_manifest()

  expect_error(
    downsample_immundata(idata, n = 1, seed = 321),
    "ambiguous"
  )
})

test_that("downsample_immundata validates n and handles no-repertoire fallback", {
  idata <- get_test_idata_tsv_no_manifest()

  expect_error(
    downsample_immundata(idata, n = 2.5),
    "integer count"
  )

  n_before <- idata$annotations |>
    distinct(imd_barcode) |>
    collect() |>
    nrow()

  ds <- downsample_immundata(idata, n = 0.1, seed = 1)

  n_after <- ds$annotations |>
    distinct(imd_barcode) |>
    collect() |>
    nrow()

  expect_equal(n_after, floor(n_before * 0.1))
  expect_null(ds$repertoires)
})

test_that("downsample_immundata warns and keeps repertoire unchanged when n exceeds available units", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    cell_id = c("c1", "c2", "c3", "c4", "c5", "c6"),
    sample_id = c("S1", "S1", "S1", "S2", "S2", "S2"),
    v_call = c("IGHV1", "IGHV2", "IGHV3", "IGHV1", "IGHV2", "IGHV3"),
    j_call = c("IGHJ1", "IGHJ2", "IGHJ3", "IGHJ1", "IGHJ2", "IGHJ3"),
    junction_aa = c("A1", "A2", "A3", "B1", "B2", "B3"),
    locus = "IGH",
    umi_count = c(1, 1, 1, 1, 1, 1)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(features = c("v_call", "j_call", "junction_aa"), chains = "IGH"),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "downsample n exceeds units"
  )

  reps_before <- idata$repertoires |>
    arrange(sample_id)

  ds <- NULL
  expect_warning(
    ds <- downsample_immundata(idata, n = 10, seed = 42),
    "returned unchanged"
  )

  reps_after <- ds$repertoires |>
    arrange(sample_id)

  annotation_counts <- ds$annotations |>
    select(sample_id, imd_barcode) |>
    distinct() |>
    collect() |>
    summarise(.by = sample_id, n_barcodes = n()) |>
    arrange(sample_id)

  expect_equal(reps_after$n_barcodes, reps_before$n_barcodes)
  expect_equal(annotation_counts$n_barcodes, reps_before$n_barcodes)
})

test_that("downsample_immundata supports count-mode proportion downsampling", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    sample_id = c("S1", "S1", "S2", "S2"),
    v_call = c("TRBV1", "TRBV2", "TRBV3", "TRBV4"),
    j_call = c("TRBJ1", "TRBJ2", "TRBJ1", "TRBJ2"),
    junction_aa = c("AAAA", "BBBB", "CCCC", "DDDD"),
    clone_count = c(8, 7, 6, 9)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = c("v_call", "j_call", "junction_aa"),
    count_col = "clone_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "downsample count proportion"
  )

  ds <- downsample_immundata(idata, n = 0.5, seed = 101)
  reps <- ds$repertoires
  ann <- ds$annotations |>
    select(sample_id, imd_n_chains, imd_count) |>
    collect()

  count_totals <- ann |>
    summarise(.by = sample_id, n_barcodes = sum(imd_n_chains)) |>
    arrange(sample_id)

  expect_equal(sort(reps$n_barcodes), c(7, 7))
  expect_equal(count_totals$n_barcodes, c(7L, 7L))
  expect_true(all(ann$imd_count == ann$imd_n_chains))
})

test_that("downsample_immundata is deterministic in count mode with seed", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    sample_id = c("S1", "S1", "S2", "S2"),
    v_call = c("TRBV1", "TRBV2", "TRBV3", "TRBV4"),
    j_call = c("TRBJ1", "TRBJ2", "TRBJ1", "TRBJ2"),
    junction_aa = c("AAAA", "BBBB", "CCCC", "DDDD"),
    clone_count = c(8, 7, 6, 9)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = c("v_call", "j_call", "junction_aa"),
    count_col = "clone_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "downsample count deterministic"
  )

  ds1 <- downsample_immundata(idata, n = 5, seed = 222)
  ds2 <- downsample_immundata(idata, n = 5, seed = 222)

  ann1 <- ds1$annotations |>
    select(sample_id, imd_barcode, imd_n_chains) |>
    arrange(sample_id, imd_barcode) |>
    collect()
  ann2 <- ds2$annotations |>
    select(sample_id, imd_barcode, imd_n_chains) |>
    arrange(sample_id, imd_barcode) |>
    collect()

  expect_equal(ann1, ann2)
})

test_that("downsample_immundata errors when proportion results in zero target", {
  idata <- get_test_idata_tsv_no_manifest()

  expect_error(
    downsample_immundata(idata, n = 0.0001, seed = 1),
    "Increase `n`"
  )
})

test_that("downsample_immundata produces consistent imd_count and imd_proportion invariants", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    cell_id = c("c1", "c2", "c3", "c4", "c5", "c6"),
    sample_id = c("S1", "S1", "S1", "S2", "S2", "S2"),
    v_call = c("IGHV1", "IGHV1", "IGHV2", "IGHV1", "IGHV2", "IGHV2"),
    j_call = c("IGHJ1", "IGHJ1", "IGHJ2", "IGHJ1", "IGHJ2", "IGHJ2"),
    junction_aa = c("A1", "A1", "A2", "B1", "B2", "B2"),
    locus = "IGH",
    umi_count = c(10, 11, 12, 13, 14, 15)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(features = c("v_call", "j_call", "junction_aa"), chains = "IGH"),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )
  idata <- agg_repertoires_with_integrity(
    idata,
    schema = "sample_id",
    context = "downsample count/proportion invariants"
  )

  ds <- downsample_immundata(idata, n = 2, seed = 33)

  reps <- ds$repertoires
  ann <- ds$annotations |> collect()

  receptor_stats <- ann |>
    select(imd_repertoire_id, imd_receptor_id, imd_count, imd_proportion) |>
    distinct() |>
    summarise(
      .by = imd_repertoire_id,
      sum_count = sum(imd_count),
      sum_prop = sum(imd_proportion)
    ) |>
    arrange(imd_repertoire_id)

  reps <- reps |> arrange(imd_repertoire_id)

  expect_equal(receptor_stats$sum_count, reps$n_barcodes)
  expect_true(all(abs(receptor_stats$sum_prop - 1) < 1e-8))
})
