test_that("filter_receptors() filters ImmunData by receptor-level conditions", {
  outdir <- tempdir()

  md_path <- system.file("extdata", "metadata_samples.tsv", package = "immundata")
  metadata_df <- read_metadata(md_path)

  sample_files <- c(
    system.file("extdata", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )
  idata <- read_repertoires(
    path = sample_files,
    schema = c("cdr3_aa", "v_call"),
    metadata = metadata_df,
    output_folder = outdir
  )

  # Sanity check
  checkmate::expect_r6(idata, "ImmunData")

  idata_filtered <- filter_receptors(idata, v_call == "TRBV7-9*01")
  checkmate::expect_r6(idata_filtered, "ImmunData")

  v_vals <- idata_filtered$receptors %>%
    distinct(v_call) %>%
    pull(v_call)
  expect_true(all(v_vals == "TRBV7-9*01"), info = "Receptors table should only have TRBV12-1 after filter.")

  # Confirm that annotations are correspondingly smaller
  expect_lte(idata_filtered$annotations |> collect() |> nrow(), idata$annotations |> collect() |> nrow())
})

test_that("filter_annotations() filters ImmunData by annotation-level conditions (locus)", {
  outdir <- tempdir()

  sample_file <- system.file("extdata", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = outdir
  )

  # Let's say the annotation table has a column "locus" (common in TCR/BCR data)
  # We'll filter to "TRB". Adjust to an actual locus present in your data
  filtered <- filter_annotations(idata, locus == "TCRB")
  checkmate::expect_r6(filtered, classes = "ImmunData")

  # Confirm all `locus` values in annotations are "TRB"
  unique_locus <- filtered$annotations %>%
    distinct(locus) %>%
    pull(locus)
  expect_equal(unique(unique_locus), "TCRB")

  # The receptor table should be smaller or the same size, never bigger
  expect_lte(filtered$receptors |> collect() |> nrow(), idata$receptors |> collect() |> nrow())
})

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
