test_that("exact matching with single and multiple patterns", {
  idata <- get_test_idata_tsv_no_manifest()
  all_receptors <- idata$receptors %>% collect()

  # Single pattern
  pat1 <- all_receptors$cdr3_aa[1]
  out1 <- filter_immundata(idata, seq_options = make_seq_options(
    query_col = "cdr3_aa",
    patterns = pat1,
    method = "exact", name_type = "pattern"
  ))
  gold1 <- all_receptors %>% filter(cdr3_aa == pat1)

  expect_equal(out1$receptors %>% collect() %>% arrange(cdr3_aa) |> nrow(), gold1 |> nrow())
  expect_equal(
    out1$receptors %>% collect() %>% arrange(cdr3_aa),
    gold1 %>% arrange(cdr3_aa)
  )

  # Multiple patterns
  pats <- unique(all_receptors$cdr3_aa)[1:2]
  out2 <- filter_immundata(idata, seq_options = make_seq_options(
    query_col = "cdr3_aa",
    patterns = pats,
    method = "exact", name_type = "pattern"
  ))

  gold2 <- all_receptors %>% filter(cdr3_aa %in% pats)
  expect_equal(
    out2$receptors %>% collect() %>% nrow(),
    gold2 %>% nrow()
  )
  expect_equal(
    out2$receptors %>% collect() %>% arrange(cdr3_aa),
    gold2 %>% arrange(cdr3_aa)
  )
})

test_that("exact matching preserves preceding annotation filters", {
  annotations <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      imd_receptor_id = rep(1L, 4),
      imd_barcode = c("bc1", "bc1", "bc2", "bc2"),
      imd_chain_id = 1:4,
      imd_n_chains = rep(1L, 4),
      locus = rep(c("IGH", "IGL"), 2),
      cdr3_aa = rep(c("AAA", "CCC"), 2),
      sample_id = rep(c("S1", "S2"), each = 2)
    )
  )
  idata <- ImmunData$new(
    schema = make_receptor_schema(
      features = "cdr3_aa",
      chains = c("IGH", "IGL")
    ),
    annotations = annotations
  )

  out <- filter_immundata(
    idata,
    sample_id == "S1",
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = "AAA",
      method = "exact"
    ),
    keep_repertoires = FALSE
  )

  actual <- out$annotations |>
    dplyr::collect() |>
    dplyr::arrange(imd_chain_id)

  expect_equal(nrow(actual), 2L)
  expect_setequal(actual$sample_id, "S1")
  expect_setequal(actual$locus, c("IGH", "IGL"))
  expect_setequal(actual$cdr3_aa, c("AAA", "CCC"))
})
