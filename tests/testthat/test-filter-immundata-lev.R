# 3. Levenshtein fuzzy matching
testthat::test_that("Levenshtein fuzzy matching returns correct results", {
  idata <- get_test_idata_tsv_no_manifest()
  all_receptors <- idata$receptors |> collect()

  pat <- substr(all_receptors$cdr3_aa[1:3], 1, nchar(all_receptors$cdr3_aa[1:3]) - 1)
  maxd <- 1
  out <- filter_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns  = pat,
      method    = "lev",
      max_dist  = maxd,
      name_type = "pattern"
    )
  )
  dists <- adist(all_receptors$cdr3_aa, pat)
  gold <- all_receptors[apply(dists, 1, min) <= maxd, ]

  expect_equal(
    out$receptors |> collect() |> arrange(),
    gold |> arrange()
  )
})

# 6. Combined pre-filter and fuzzy matching
test_that("combined pre-filter and fuzzy matching works correctly", {
  idata <- get_test_idata_tsv_no_manifest()
  all_receptors <- idata$receptors |> collect()

  vc <- all_receptors$v_call[5]
  pat <- substr(all_receptors$cdr3_aa[5], 1, nchar(all_receptors$cdr3_aa[5]) - 1)
  maxd <- 1
  out <- filter_immundata(
    idata,
    v_call == vc,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns  = pat,
      method    = "lev",
      max_dist  = maxd,
      name_type = "pattern"
    )
  )
  sub <- all_receptors |> filter(v_call == vc)
  dists <- adist(sub$cdr3_aa, pat)
  gold <- sub[dists <= maxd, ]
  expect_equal(
    out$receptors |> collect() |> arrange(cdr3_aa),
    gold |> arrange(cdr3_aa)
  )
})

test_that("Levenshtein matching preserves paired chains within preceding filters", {
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
      method = "lev",
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
