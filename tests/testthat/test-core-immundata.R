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

test_that("receptor schema validators accept only valid schema structures", {
  valid_schema <- make_receptor_schema(
    features = c("junction_aa", "v_call"),
    chains = c("TRA", "TRB")
  )

  expect_true(assert_receptor_schema("junction_aa"))
  expect_true(assert_receptor_schema(valid_schema))
  expect_true(test_receptor_schema("junction_aa"))
  expect_true(test_receptor_schema(valid_schema))

  expect_false(test_receptor_schema(list(features = 1, chains = 2)))
  expect_false(test_receptor_schema(list(features = "junction_aa")))
  expect_error(assert_receptor_schema(list(features = 1, chains = 2)))
})

test_that("snapshot file constants contain only files that are written", {
  expect_identical(
    imd_files(),
    list(
      metadata = "metadata.json",
      annotations = "annotations.parquet"
    )
  )
})
