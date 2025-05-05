testthat::test_that("annotate_receptors adds receptor‑level annotations", {
  idata <- get_test_idata_tsv_no_metadata()
  receptor_id_col <- imd_schema()$receptor

  recs <- idata$receptors %>%
    select(!!sym(receptor_id_col), cdr3_aa) %>%
    collect() |> head(5)

  ann <- tibble(
    sequence_id = recs[[receptor_id_col]],
    receptor_seq = paste0("ANN_", recs$cdr3_aa),
    annot_field  = paste0("annotation", 1:nrow(recs))
  )
  colnames(ann) <- c(receptor_id_col, colnames(ann)[2:ncol(ann)])

  out <- annotate_receptors(
    idata,
    annotations = ann
  )

  actual_annot <- out$annotations |>
    collect() |>
    arrange(across(everything()))
  expected_annot <- idata$annotations |>
    collect() |>
    left_join(ann, by = join_by(imd_receptor_id == imd_receptor_id)) |>
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
