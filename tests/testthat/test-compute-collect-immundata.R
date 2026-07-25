make_compute_state_test_idata <- function() {
  annotations <- tibble::tibble(
    imd_receptor_id = 1:4,
    imd_barcode = paste0("bc", 1:4),
    imd_chain_id = 1:4,
    imd_n_chains = 1L,
    cdr3_aa = c("AAA", "AAT", "AAAA", "BBB"),
    sample_id = c("S1", "S1", "S2", "S2")
  )

  ImmunData$new(
    schema = "cdr3_aa",
    annotations = duckplyr::as_duckdb_tibble(annotations)
  )
}

test_that("compute() preserves repertoire, strata, and provenance state", {
  idata <- get_test_immundata(repertoire_schema = c("Response", "Therapy")) |>
    agg_strata("Response")

  reps_before <- idata$repertoires
  strata_before <- idata$strata
  prov_before <- imd_get_provenance(idata)
  out <- compute(idata)

  checkmate::expect_r6(out, classes = "ImmunData")

  in_annotations <- idata$annotations |> collect()
  out_annotations <- out$annotations |> collect()

  expect_equal(nrow(out_annotations), nrow(in_annotations))
  expect_equal(colnames(out_annotations), colnames(in_annotations))
  expect_equal(out$repertoires, reps_before)
  expect_equal(out$strata, strata_before)
  expect_equal(out$schema_repertoire, idata$schema_repertoire)
  expect_equal(out$schema_strata, idata$schema_strata)
  expect_equal(
    imd_get_provenance(out)[sort(names(imd_get_provenance(out)))],
    prov_before[sort(names(prov_before))]
  )
})

test_that("compute() supports repertoire-free and strata-free state", {
  annotations_only <- make_compute_state_test_idata()
  annotations_only_out <- compute(annotations_only)

  expect_null(annotations_only_out$repertoires)
  expect_null(annotations_only_out$strata)
  expect_null(annotations_only_out$schema_repertoire)
  expect_null(annotations_only_out$schema_strata)

  repertoires_only <- annotations_only |>
    agg_repertoires("sample_id")
  repertoires_only_out <- compute(repertoires_only)

  expect_equal(repertoires_only_out$repertoires, repertoires_only$repertoires)
  expect_equal(
    repertoires_only_out$schema_repertoire,
    repertoires_only$schema_repertoire
  )
  expect_null(repertoires_only_out$strata)
  expect_null(repertoires_only_out$schema_strata)
})

test_that("collect() on ImmunData returns a tibble without factor columns", {
  idata <- get_test_idata()

  annotations <- idata$annotations |> collect()
  annotations$test_factor <- factor("x")

  idata_with_factor <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = annotations
  )

  out <- collect(idata_with_factor)

  expect_s3_class(out, "tbl_df")
  expect_false(any(vapply(out, is.factor, logical(1))))
  expect_true(is.character(out$test_factor))
})

test_that("colnames() on ImmunData returns annotation column names", {
  idata <- get_test_idata()

  expect_equal(colnames(idata), colnames(idata$annotations))
})

test_that("renaming via names/dimnames/colnames is blocked for ImmunData", {
  idata <- get_test_idata()

  expect_error(
    {
      colnames(idata) <- paste0("x", seq_along(colnames(idata)))
    },
    "not allowed"
  )

  expect_error(
    {
      dimnames(idata) <- list(NULL, paste0("y", seq_along(colnames(idata))))
    },
    "not allowed"
  )

  expect_error(
    {
      names(idata) <- letters[seq_along(names(idata))]
    },
    "not allowed"
  )
})
