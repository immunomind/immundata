testthat::test_that("Regex matching returns correct results", {
  idata <- get_test_idata_tsv_no_manifest()
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

test_that("regex filtering uses unique temporary column names", {
  query_col <- "imd_sim_regex_1"
  path <- file.path(withr::local_tempdir(), "repertoires.parquet")
  duckplyr::duckdb_tibble(imd_sim_regex_1 = c("AB", "AC", "AD")) |>
    duckplyr::compute_parquet(path)
  idata <- read_repertoires(
    path,
    schema = query_col,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL,
    repertoire_schema = NULL
  )

  out <- filter_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = query_col,
      patterns = c("A[BC]", "A(BC)"),
      method = "regex",
      name_type = "pattern"
    )
  ) |>
    collect()

  expect_setequal(out[[query_col]], c("AB", "AC"))
})
