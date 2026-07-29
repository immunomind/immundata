test_that("filter() filters ImmunData by receptor-level conditions", {
  idata <- get_test_idata_tsv_with_manifest()

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
  idata <- get_test_idata_tsv_no_manifest()

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

test_that("filters discard repertoire state when not preserving repertoires", {
  annotations <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      imd_receptor_id = 1:4,
      imd_barcode = paste0("bc", 1:4),
      imd_chain_id = 1:4,
      imd_n_chains = 1L,
      cdr3_aa = c("AAA", "AAT", "AAA", "BBB"),
      sample_id = c("S1", "S1", "S2", "S2")
    )
  )
  idata <- ImmunData$new(schema = "cdr3_aa", annotations = annotations) |>
    agg_repertoires("sample_id") |>
    agg_strata("sample_id")

  repertoire_state_cols <- c(
    imd_schema("repertoire"),
    imd_schema("strata"),
    imd_schema("strata_name"),
    imd_schema("count"),
    imd_schema("proportion"),
    imd_schema("n_receptors"),
    imd_schema("n_barcodes"),
    imd_schema("n_repertoires")
  )
  expect_true(all(c(
    imd_schema("repertoire"),
    imd_schema("strata"),
    imd_schema("count"),
    imd_schema("proportion"),
    imd_schema("n_repertoires")
  ) %in% names(idata$annotations)))

  filtered <- list(
    filter_immundata(idata, sample_id == "S1", keep_repertoires = FALSE),
    filter_barcodes(idata, "bc1", keep_repertoires = FALSE),
    filter_receptors(idata, 1L, keep_repertoires = FALSE)
  )

  for (out in filtered) {
    expect_null(out$repertoires)
    expect_null(out$schema_repertoire)
    expect_null(out$schema_strata)
    expect_false(any(repertoire_state_cols %in% names(out$annotations)))
  }
})

test_that("filters rebuild strata and retain strata labels", {
  annotations <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      imd_receptor_id = 1:6,
      imd_barcode = paste0("bc", 1:6),
      imd_chain_id = 1:6,
      imd_n_chains = 1L,
      cdr3_aa = c("AAA", "AAT", "ABB", "BBB", "BBC", "BCC"),
      sample_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
      response = c("R", "R", "NR", "NR", "R", "R")
    )
  )
  idata <- ImmunData$new(schema = "cdr3_aa", annotations = annotations) |>
    agg_repertoires(c("sample_id", "response")) |>
    agg_strata("response")

  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")
  strata_names <- ifelse(
    idata$strata$response == "R",
    "Responder",
    "Non-responder"
  )
  idata <- rename_strata(
    idata,
    stats::setNames(strata_names, as.character(idata$strata[[strata_col]]))
  )

  filtered <- list(
    filter_immundata(idata, sample_id != "S3"),
    filter_barcodes(idata, paste0("bc", 1:4)),
    filter_receptors(idata, 1:4)
  )

  for (out in filtered) {
    expect_equal(out$schema_strata, "response")
    expect_true(strata_col %in% names(out$annotations))
    expect_true(all(c(strata_col, strata_name_col) %in% names(out$repertoires)))

    rebuilt_strata <- out$strata |>
      arrange(response)
    expect_equal(rebuilt_strata$response, c("NR", "R"))
    expect_equal(
      rebuilt_strata[[strata_name_col]],
      c("Non-responder", "Responder")
    )
  }
})
