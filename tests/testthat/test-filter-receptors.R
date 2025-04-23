test_that("filter_receptors() filters ImmunData by a set of receptor identifiers", {
  outdir <- tempdir()

  sample_file <- system.file("extdata", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = outdir
  )

  all_receptors <- idata$annotations %>%
    distinct(imd_receptor_id) %>%
    pull(imd_receptor_id)

  some_receptors <- head(all_receptors, 5)

  filtered <- filter_receptors(idata, some_receptors)
  checkmate::expect_r6(filtered, classes = "ImmunData")

  new_receptors <- filtered$annotations %>%
    distinct(imd_receptor_id) %>%
    pull(imd_receptor_id)

  expect_equal(sort(new_receptors), sort(some_receptors))

  expect_lte(filtered$receptors |> collect() |> nrow(), idata$receptors |> collect() |> nrow())
})
