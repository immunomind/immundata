test_that("agg_receptors() excludes incomplete receptor features", {
  dataset <- duckplyr::duckdb_tibble(
    junction_aa = c("CASSA", "CASSB", NA_character_, "", "  "),
    j_call = c("TRBJ1", NA_character_, "TRBJ2", "TRBJ3", "TRBJ4"),
    v_call = c(NA_character_, "TRBV2", "TRBV3", "TRBV4", "TRBV5"),
    .prudence = "stingy"
  )

  result <- agg_receptors(
    dataset,
    schema = c("junction_aa", "j_call"),
    verbose = FALSE
  ) |>
    collect()

  expect_equal(result$junction_aa, "CASSA")
  expect_true(is.na(result$v_call))
})

test_that("paired aggregation selects complete chains before highest UMI", {
  dataset <- duckplyr::duckdb_tibble(
    cell = c(
      "complete", "complete",
      "broken", "broken",
      "fallback", "fallback", "fallback"
    ),
    locus = c(
      "TRA", "TRB",
      "TRA", "TRB",
      "TRA", "TRB", "TRB"
    ),
    junction_aa = c(
      "TRA1", "TRB1",
      "TRA2", NA_character_,
      "TRA3", NA_character_, "TRB3"
    ),
    umi = c(10, 10, 10, 10, 10, 20, 5),
    .prudence = "stingy"
  )

  result <- agg_receptors(
    dataset,
    schema = make_receptor_schema(
      features = "junction_aa",
      chains = c("TRA", "TRB")
    ),
    barcode_col = "cell",
    locus_col = "locus",
    umi_col = "umi",
    verbose = FALSE
  ) |>
    collect()

  expect_setequal(unique(result$imd_barcode), c("complete", "fallback"))
  expect_equal(
    result |>
      filter(imd_barcode == "fallback", locus == "TRB") |>
      pull(junction_aa),
    "TRB3"
  )
})
