test_that("Regex matching returns correct results", {
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

  regex <- paste0('^', substr(all_receptors$cdr3_aa[1], 1, 4))

  out <- filter_receptors(
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

