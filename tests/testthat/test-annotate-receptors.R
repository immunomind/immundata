testthat::test_that("annotate_receptors adds receptor‑level annotations", {
  idata <- get_test_idata_tsv_no_manifest()
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

  actual_annot <- out$annotations |> collect()
  expected_annot <- idata$annotations |>
    collect() |>
    left_join(ann, by = join_by(imd_receptor_id == imd_receptor_id))

  expect_equal(
    actual_annot |> count(),
    expected_annot |> count()
  )

  expect_equal(
    sort(colnames(actual_annot)),
    sort(c(colnames(idata$annotations), "receptor_seq", "annot_field"))
  )

  expect_equal(
    actual_annot |>
      select(all_of(sort(colnames(actual_annot)))) |>
      arrange(across(everything())),
    expected_annot |>
      select(all_of(sort(colnames(expected_annot)))) |>
      arrange(across(everything()))
  )
})

testthat::test_that("annotate_receptors preserves join column order without repertoires", {
  idata <- get_test_idata_tsv_no_manifest(repertoire_schema = NULL)
  receptor_id_col <- imd_schema()$receptor

  recs <- idata$receptors |>
    select(!!sym(receptor_id_col), cdr3_aa) |>
    collect() |>
    head(5)
  ann <- tibble(
    imd_receptor_id = recs[[receptor_id_col]],
    receptor_seq = paste0("ANN_", recs$cdr3_aa)
  )

  actual <- annotate_receptors(idata, annotations = ann)$annotations |>
    collect()
  expected <- idata$annotations |>
    collect() |>
    left_join(ann, by = "imd_receptor_id")

  expect_null(idata$repertoires)
  expect_equal(actual, expected)
})
