test_that("agg_receptors standardizes a custom locus column before filtering", {
  dataset <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      cell_id = c("cell_1", "cell_2", "cell_3"),
      chain_locus = c("IGH", "IGL", "IGH"),
      v_call = c("IGHV1", "IGLV1", "IGHV2"),
      j_call = c("IGHJ1", "IGLJ1", "IGHJ2"),
      junction_aa = c("CARW", "CAKW", "CARG"),
      umi_count = c(10L, 10L, 10L)
    )
  )

  actual <- agg_receptors(
    dataset = dataset,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = "IGH"
    ),
    barcode_col = "cell_id",
    locus_col = "chain_locus",
    umi_col = "umi_count"
  ) |>
    dplyr::collect()

  expect_true("locus" %in% names(actual))
  expect_false("chain_locus" %in% names(actual))
  expect_setequal(actual$locus, "IGH")
  expect_setequal(actual$cell_id, c("cell_1", "cell_3"))
})

test_that("agg_receptors rejects custom and canonical locus columns together", {
  dataset <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      chain_locus = "IGH",
      locus = "IGL",
      junction_aa = "CARW"
    )
  )

  expect_error(
    agg_receptors(
      dataset = dataset,
      schema = make_receptor_schema(
        features = "junction_aa",
        chains = "IGH"
      ),
      locus_col = "chain_locus"
    ),
    "both the custom locus column.*chain_locus.*canonical locus column.*locus"
  )
})

test_that("distance filtering treats quoted patterns as literal values", {
  annotations <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      imd_receptor_id = c(1L, 2L),
      imd_barcode = c("bc_1", "bc_2"),
      imd_chain_id = c(1L, 2L),
      imd_n_chains = c(1L, 1L),
      cdr3_aa = c("CA'RW", "CARRW")
    )
  )
  idata <- ImmunData$new(schema = "cdr3_aa", annotations = annotations)

  for (method in c("lev", "hamm")) {
    annotated <- mutate_immundata(
      idata,
      seq_options = make_seq_options(
        query_col = "cdr3_aa",
        patterns = "CA'RW",
        method = method
      )
    )$annotations |>
      dplyr::collect() |>
      dplyr::arrange(imd_receptor_id)

    distance_col <- paste0("imd_sim_", method, "_1")
    expect_equal(annotated[[distance_col]], c(0L, 1L), info = method)

    filtered <- filter_immundata(
      idata,
      seq_options = make_seq_options(
        query_col = "cdr3_aa",
        patterns = "CA'RW",
        method = method,
        max_dist = 0L
      ),
      keep_repertoires = FALSE
    )$receptors |>
      dplyr::collect()

    expect_equal(filtered$cdr3_aa, "CA'RW", info = method)
  }
})
