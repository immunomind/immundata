test_that("filter_barcodes() filters ImmunData by a set of cell barcodes", {
  outdir <- tempdir()

  idata <- get_test_idata_tsv_no_metadata()

  barcode_col <- imd_schema_sym("barcode")

  # Suppose the annotation table has a column "imd_cell_id" with barcodes
  # We'll pick the first few for demonstration
  all_cells <- idata$annotations %>%
    distinct(!!barcode_col) %>%
    pull(!!barcode_col)

  # If there are at least 5 cells, let's pick a subset
  some_cells <- head(all_cells, 5)

  filtered <- filter_barcodes(idata, some_cells)
  checkmate::expect_r6(filtered, classes = "ImmunData")

  # The new annotation table should only have those 5
  new_cells <- filtered$annotations %>%
    distinct(!!barcode_col) %>%
    pull(!!barcode_col)

  expect_equal(sort(new_cells), sort(some_cells))

  # The receptor table should only have the receptor IDs used by these cells
  expect_lte(filtered$receptors |> collect() |> nrow(), idata$receptors |> collect() |> nrow())
})
