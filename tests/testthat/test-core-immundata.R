test_that("ImmunData validates its annotations input", {
  valid_annotations <- duckplyr::as_duckdb_tibble(
    tibble::tibble(
      imd_receptor_id = 1L,
      imd_barcode = "bc1",
      imd_chain_id = 1L,
      imd_n_chains = 1L,
      cdr3_aa = "AAA"
    )
  )

  expect_s3_class(
    ImmunData$new(schema = "cdr3_aa", annotations = valid_annotations),
    "ImmunData"
  )

  expect_error(
    ImmunData$new(schema = "cdr3_aa", annotations = list(cdr3_aa = "AAA")),
    "data[.]frame"
  )
})
