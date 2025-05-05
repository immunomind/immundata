testthat::test_that("Regex matching returns correct results", {
  idata <- get_test_idata_tsv_no_metadata()
  all_receptors <- idata$receptors %>% collect()

  regex <- paste0("^", substr(all_receptors$cdr3_aa[1], 1, 4))

  out <- filter_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns  = regex,
      method    = "regex"
    )
  )
  gold <- all_receptors %>% filter(grepl(regex, cdr3_aa))

  expect_equal(
    out$receptors %>% collect() %>% nrow(),
    gold %>% nrow()
  )

  expect_equal(
    out$receptors %>% collect() %>% arrange(),
    gold %>% arrange()
  )
})
