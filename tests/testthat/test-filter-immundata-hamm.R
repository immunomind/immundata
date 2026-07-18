# 4. Hamming fuzzy matching
test_that("Hamming fuzzy matching returns correct results", {
  idata <- get_test_idata_tsv_no_manifest()
  all_receptors <- idata$receptors %>% collect()

  orig <- all_receptors$cdr3_aa[1]
  pat <- paste0("X", substring(orig, 2))
  maxd <- 1
  out_receptors <- filter_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns  = pat,
      method    = "hamm",
      max_dist  = maxd,
      name_type = "pattern"
    )
  )$receptors |> collect()
  mask_len <- nchar(all_receptors$cdr3_aa) == nchar(pat)
  sub <- all_receptors[mask_len, ]
  dists <- adist(sub$cdr3_aa, pat)
  gold <- sub[dists <= maxd, ]
  expect_equal(
    out_receptors %>% nrow(),
    gold %>% nrow()
  )
  expect_equal(
    out_receptors %>% arrange(cdr3_aa),
    gold %>% arrange(cdr3_aa)
  )
})

test_that("Hamming matching preserves paired chains within preceding filters", {
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

  actual <- filter_immundata(
    idata,
    sample_id == "S1",
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = "AAT",
      method = "hamm",
      max_dist = 1L
    ),
    keep_repertoires = FALSE
  )$annotations |>
    dplyr::collect() |>
    dplyr::arrange(imd_chain_id) |>
    dplyr::select(imd_chain_id, locus, cdr3_aa, sample_id)

  expected <- tibble::tibble(
    imd_chain_id = 1:2,
    locus = c("IGH", "IGL"),
    cdr3_aa = c("AAA", "CCC"),
    sample_id = c("S1", "S1")
  )

  expect_equal(actual, expected)
})
