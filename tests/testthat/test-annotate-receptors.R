test_that("annotate_receptors adds receptor‑level annotations by identifier or feature", {
  idata <- get_test_idata()
  receptor_id_col <- imd_schema()$receptor

  recs <- idata$receptors %>%
    select(!!sym(receptor_id_col), cdr3_aa) %>%
    collect() |> head(5)

  ann <- tibble(
    receptor_id  = recs[[receptor_id_col]],
    receptor_seq = recs$cdr3_aa,
    annot_field  = paste0("annotation", 1:nrow(recs))
  )


  #
  # Test matching by identifier
  #
  out <- annotate_receptors(
    idata,
    annotations = ann,
    match_col = c(idata = receptor_id_col, annotations = "receptor_id")
  )

  expect_equal(
    colnames(out$receptors),
    colnames(idata$receptors)
  )
  expect_equal(
    collect(out$receptors),
    collect(idata$receptors)
  )

  #
  # Test matching by sequence
  #
  print(out)

  # annotations slot must have new column 'receptor_seq2'
  expect_true("receptor_seq2" %in% colnames(out$annotations))

  # for each receptor in ann, all annotation rows with that receptor get the correct seq
  for (i in seq_len(nrow(ann))) {
    rid <- ann$receptor_id[i]
    seq2 <- ann$receptor_seq2[i]
    got <- out$annotations %>%
      filter(!!sym(receptor_id_col) == rid) %>%
      pull(receptor_seq2) %>%
      unique()
    expect_equal(got, seq2)
  }
})
