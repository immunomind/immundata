test_that("compute() on ImmunData returns ImmunData with computed annotations", {
  idata <- get_test_idata()
  out <- compute(idata)

  checkmate::expect_r6(out, classes = "ImmunData")

  in_annotations <- idata$annotations |> collect()
  out_annotations <- out$annotations |> collect()

  expect_equal(nrow(out_annotations), nrow(in_annotations))
  expect_equal(colnames(out_annotations), colnames(in_annotations))
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
