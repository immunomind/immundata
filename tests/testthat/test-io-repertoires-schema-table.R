test_that("Case 1: read_repertoires() handles table case correctly", {
  small_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  # Create a fresh temp folder
  outdir <- file.path(tempdir(), "test-no-barcodes")
  dir.create(outdir, showWarnings = FALSE)

  schema_features <- c("cdr3_aa", "v_call")

  idata <- read_repertoires(
    path = small_file,
    schema = schema_features,
    output_folder = outdir
  )

  annotations <- idata$annotations |> collect()
  receptors <- idata$receptors |> collect()

  expect_false(nrow(annotations) == 0)

  expect_setequal(colnames(idata$receptors), c(schema_features, imd_schema("receptor")))

  expect_equal(receptors |> select(-imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))
  expect_equal(receptors |> select(imd_receptor_id) |> distinct() |> nrow(), nrow(receptors))

  checkmate::expect_r6(idata, classes = "ImmunData")

  expect_true(file.exists(file.path(outdir, imd_files()$metadata)))
  expect_true(file.exists(file.path(outdir, imd_files()$annotations)))
})
