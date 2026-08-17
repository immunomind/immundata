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

test_that("Levenshtein filtering keeps edits at sequence ends", {
  sequences <- tibble::tibble(
    cdr3_aa = c("AAAAA", "BAAAA", "AAAAB", "AAAA", "AAAAAA", "BBAAA")
  ) |>
    duckplyr::as_duckdb_tibble()

  actual <- annotate_tbl_distance(
    sequences,
    query_col = "cdr3_aa",
    patterns = "AAAAA",
    method = "lev",
    max_dist = 1,
    name_type = "index"
  ) |>
    collect() |>
    arrange(cdr3_aa)

  expect_equal(
    actual$cdr3_aa,
    sort(c("AAAAA", "BAAAA", "AAAAB", "AAAA", "AAAAAA"))
  )
  expect_equal(actual$imd_sim_lev_1, rep(1, 5) - (actual$cdr3_aa == "AAAAA"))
})

test_that("distance materialization is independent of the R random seed", {
  sequences <- tibble::tibble(cdr3_aa = c("AAAAA", "AAAAB")) |>
    duckplyr::as_duckdb_tibble()

  set.seed(1)
  first <- annotate_tbl_distance(
    sequences,
    query_col = "cdr3_aa",
    patterns = "AAAAA",
    method = "lev"
  )

  set.seed(1)
  second <- annotate_tbl_distance(
    sequences,
    query_col = "cdr3_aa",
    patterns = "AAAAA",
    method = "lev"
  )

  expect_equal(
    collect(first) |> arrange(cdr3_aa),
    collect(second) |> arrange(cdr3_aa)
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
  idata <- make_paired_filter_test_idata()

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
