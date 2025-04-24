test_that("exact matching with single and multiple patterns", {
  sample_files <- c(
    system.file("extdata", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )
  idata <- read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    output_folder = tempdir()
  )
  all_receptors <- idata$receptors %>% collect()

  # Single pattern
  pat1 <- all_receptors$cdr3_aa[1]
  out1 <- filter(idata, seq_options = make_seq_options(
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
  out2 <- filter(idata, seq_options = make_seq_options(
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
