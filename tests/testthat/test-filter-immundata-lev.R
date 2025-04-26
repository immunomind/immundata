# 3. Levenshtein fuzzy matching
testthat::test_that("Levenshtein fuzzy matching returns correct results", {
  sample_files <- c(
    system.file("extdata", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )
  idata <- read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    output_folder = tempdir()
  )
  all_receptors <- idata$receptors |> collect()

  pat <- substr(all_receptors$cdr3_aa[1:3], 1, nchar(all_receptors$cdr3_aa[1:3]) - 1)
  maxd <- 1
  out <- filter(
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
  sample_files <- c(
    system.file("extdata", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )
  idata <- read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    output_folder = tempdir()
  )
  all_receptors <- idata$receptors |> collect()

  vc <- all_receptors$v_call[5]
  pat <- substr(all_receptors$cdr3_aa[5], 1, nchar(all_receptors$cdr3_aa[5]) - 1)
  maxd <- 1
  out <- filter(
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
