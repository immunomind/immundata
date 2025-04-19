test_that("annotate_cells adds cell‑level annotations and enforces remove_limit", {
  idata <- get_test_idata()
  cell_id_col <- imd_schema()$cell

  # collect annotation table to data.frame to use as input
  ann_df <- idata$annotations %>% collect()
  # pick 5 barcodes
  barcodes <- ann_df[[cell_id_col]][1:5]
  # build a small annotation table with a new flag
  cell_ann <- data.frame(
    barcode = barcodes,
    new_flag = letters[1:5],
    stringsAsFactors = FALSE
  )

  # explicit match_col
  out <- annotate_cells(
    idata,
    annotations = cell_ann,
    match_col = c(idata = cell_id_col, annotations = "barcode")
  )

  # receptors table unchanged
  expect_equal(
    collect(out$receptors),
    collect(idata$receptors)
  )

  # annotations slot must have new_flag column
  expect_true("new_flag" %in% colnames(out$annotations))

  # rows matching barcodes should get the correct flag; others are NA
  got_flags <- out$annotations %>%
    select(!!sym(cell_id_col), new_flag) %>%
    collect()
  for (i in seq_along(barcodes)) {
    bc <- barcodes[i]
    flag <- cell_ann$new_flag[i]
    expect_true(all(got_flags$new_flag[got_flags[[cell_id_col]] == bc] == flag))
  }
  # rows not in cell_ann have new_flag == NA
  non_match <- setdiff(ann_df[[cell_id_col]], barcodes)
  expect_true(all(is.na(
    got_flags$new_flag[got_flags[[cell_id_col]] %in% non_match]
  )))

  # remove_limit: provide too many rows should error
  big_ann <- ann_df
  expect_error(
    annotate_cells(idata, big_ann, match_col = c(idata = cell_id_col, annotations = "."),
                   remove_limit = FALSE),
    "exceed existing idata annotations"
  )

  # but with remove_limit = TRUE it works (no error)
  expect_silent(
    annotate_cells(idata, big_ann, match_col = c(idata = cell_id_col, annotations = "<rownames>"),
                   remove_limit = TRUE)
  )
})
