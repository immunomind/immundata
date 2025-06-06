# 4. Hamming fuzzy matching
test_that("Hamming fuzzy matching returns correct results", {
  idata <- get_test_idata_tsv_no_metadata()
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
