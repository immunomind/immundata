test_that("from_immunarch() preserves typed missing values", {
  temp_dir <- tempfile("from-immunarch-input-")
  output_dir <- create_test_output_dir("from-immunarch-output-")
  on.exit(cleanup_output_dir(temp_dir), add = TRUE)
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  imm <- list(
    data = list(
      sample_1 = tibble::tibble(
        CDR3.aa = c("CASSA", "CASSB", "CASSC"),
        V.name = c("TRBV1", "TRBV2", "TRBV3"),
        J.name = c("TRBJ1", NA_character_, "NA"),
        V.end = c(12L, NA_integer_, 18L)
      )
    ),
    meta = tibble::tibble(Sample = "sample_1")
  )

  idata <- suppressWarnings(suppressMessages(from_immunarch(
    imm,
    output_folder = output_dir,
    schema = c("CDR3.aa", "V.name"),
    temp_folder = temp_dir
  )))

  annotations <- idata |>
    collect() |>
    dplyr::arrange(CDR3.aa)

  expect_type(annotations$V.end, "double")
  expect_equal(annotations$V.end, c(12, NA_real_, 18))
  expect_equal(annotations$J.name, c("TRBJ1", NA_character_, "NA"))
})
