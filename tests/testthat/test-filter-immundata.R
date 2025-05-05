test_that("filter() filters ImmunData by receptor-level conditions", {
  idata <- get_test_idata_tsv_with_metadata()

  # Sanity check
  checkmate::expect_r6(idata, "ImmunData")

  idata_filtered <- filter_immundata(idata, v_call == "TRBV7-9*01")
  checkmate::expect_r6(idata_filtered, "ImmunData")

  v_vals <- idata_filtered$receptors %>%
    distinct(v_call) %>%
    pull(v_call)
  expect_true(all(v_vals == "TRBV7-9*01"), info = "Receptors table should only have TRBV12-1 after filter.")

  # Confirm that annotations are correspondingly smaller
  expect_lte(idata_filtered$annotations |> collect() |> nrow(), idata$annotations |> collect() |> nrow())
})

test_that("filter() filters ImmunData by annotation-level conditions (locus)", {
  idata <- get_test_idata_tsv_no_metadata()

  # Let's say the annotation table has a column "locus" (common in TCR/BCR data)
  # We'll filter to "TRB". Adjust to an actual locus present in your data
  filtered <- filter_immundata(idata, locus == "TCRB")
  checkmate::expect_r6(filtered, classes = "ImmunData")

  # Confirm all `locus` values in annotations are "TRB"
  unique_locus <- filtered$annotations %>%
    distinct(locus) %>%
    pull(locus)
  expect_equal(unique(unique_locus), "TCRB")

  # The receptor table should be smaller or the same size, never bigger
  expect_lte(filtered$receptors |> collect() |> nrow(), idata$receptors |> collect() |> nrow())
})
