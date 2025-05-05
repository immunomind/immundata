test_that("filter_receptors() filters ImmunData by a set of receptor identifiers", {

  idata <- get_test_idata_tsv_no_metadata()

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
