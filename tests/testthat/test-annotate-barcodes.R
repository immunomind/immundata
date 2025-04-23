test_that("annotate_barcodes adds cell‑level annotations", {
  idata <- get_test_idata()
  cell_id_col <- imd_schema()$cell

  idata <- ImmunData$new(schema = idata$schema_receptor,
                         annotations = idata$annotations |>
                           collect() |>
                           rename(old_cell_id = !!rlang::sym(cell_id_col)) |>
                           mutate({{ cell_id_col }} := as.character(old_cell_id)) |>
                           as_duckdb_tibble())

  recs <- idata$annotations |>
    select( {{ cell_id_col }}, cdr3_aa) |>
    collect() |> head(5)

  ann <- data.frame(
    receptor_seq = paste0("ANN_", recs$cdr3_aa),
    annot_field  = paste0("annotation", 1:nrow(recs)),
    stringsAsFactors = FALSE,
    row.names = recs[[cell_id_col]]
  )

  out <- annotate_barcodes(
    idata,
    annotations = ann
  )

  ann[["cell_id"]] <- rownames(ann)
  ann <- as_tibble(ann)

  actual_annot <- out$annotations |>
    collect() |>
    arrange(across(everything()))
  expected_annot <- idata$annotations |>
    collect() |>
    left_join(ann, by = join_by(imd_barcode == cell_id)) |>
    arrange(across(everything()))

  expect_equal(
    actual_annot |> count(),
    expected_annot |> count()
  )

  expect_equal(
    sort(colnames(actual_annot)),
    sort(c(colnames(idata$annotations), "receptor_seq", "annot_field"))
  )

  expect_equal(
    actual_annot,
    expected_annot
  )
})
