test_that("filter_cells() filters ImmunData by a set of cell barcodes", {
  outdir <- tempdir()

  sample_file <- system.file("extdata", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = outdir
  )

  # Suppose the annotation table has a column "imd_cell_id" with barcodes
  # We'll pick the first few for demonstration
  all_cells <- idata$annotations %>%
    distinct(imd_cell_id) %>%
    pull(imd_cell_id)

  # If there are at least 5 cells, let's pick a subset
  some_cells <- head(all_cells, 5)

  filtered <- filter_cells(idata, some_cells)
  checkmate::expect_r6(filtered, classes = "ImmunData")

  # The new annotation table should only have those 5
  new_cells <- filtered$annotations %>%
    distinct(imd_cell_id) %>%
    pull(imd_cell_id)

  expect_equal(sort(new_cells), sort(some_cells))

  # The receptor table should only have the receptor IDs used by these cells
  expect_lte(filtered$receptors |> collect() |> nrow(), idata$receptors |> collect() |> nrow())
})
