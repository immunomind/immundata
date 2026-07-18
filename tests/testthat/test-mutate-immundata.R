make_mutate_test_idata <- function() {
  ann <- tibble::tibble(
    imd_receptor_id = 1:4,
    imd_barcode = paste0("bc", 1:4),
    imd_chain_id = 1:4,
    imd_n_chains = 1L,
    cdr3_aa = c("AAA", "AAT", "AAAA", "BBB"),
    v_call = c("V1", "V1", "V2", "V3"),
    sample_id = c("S1", "S1", "S2", "S2")
  )

  ImmunData$new(
    schema = c("cdr3_aa", "v_call"),
    annotations = duckplyr::as_duckdb_tibble(ann),
    provenance = list(
      home_path = tempdir(),
      current_path = tempdir(),
      snapshot_id = "mutate-test-snapshot",
      lineage = list(list(event = "fixture"))
    )
  )
}

test_that("mutate_immundata adds derived annotation columns without changing input", {
  idata <- make_mutate_test_idata()

  out <- mutate_immundata(
    idata,
    cdr3_len = nchar(cdr3_aa),
    receptor_label = paste(v_call, cdr3_aa, sep = ":")
  )

  expect_s3_class(out, "ImmunData")
  expect_false("cdr3_len" %in% names(idata$annotations))

  ann <- out$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(ann$cdr3_len, nchar(ann$cdr3_aa))
  expect_equal(ann$receptor_label, paste(ann$v_call, ann$cdr3_aa, sep = ":"))
})

test_that("dplyr mutate method and mutate_immundata produce equivalent annotations", {
  idata <- make_mutate_test_idata()

  direct <- mutate_immundata(idata, cdr3_len = nchar(cdr3_aa))
  s3 <- dplyr::mutate(idata, cdr3_len = nchar(cdr3_aa))

  expect_equal(
    direct$annotations |> collect() |> arrange(imd_receptor_id),
    s3$annotations |> collect() |> arrange(imd_receptor_id)
  )
})

test_that("mutate_immundata blocks system column writes", {
  idata <- make_mutate_test_idata()

  expect_error(
    mutate_immundata(idata, imd_receptor_id = 1L),
    "system columns"
  )
  expect_error(
    mutate_immundata(idata, imd_barcode = "x"),
    "system columns"
  )
  expect_error(
    mutate_immundata(idata, imd_chain_id = 1L),
    "system columns"
  )
})

test_that("mutate_immundata blocks receptor and repertoire schema writes", {
  idata <- make_mutate_test_idata() |>
    agg_repertoires("sample_id")

  expect_error(
    mutate_immundata(idata, cdr3_aa = "changed"),
    "schema columns.*cdr3_aa"
  )
  expect_error(
    mutate_immundata(idata, sample_id = "changed"),
    "schema columns.*sample_id"
  )
})

test_that("mutate_immundata blocks generated sequence schema writes", {
  idata <- make_mutate_test_idata()
  collision_idata <- ImmunData$new(
    schema = c("cdr3_aa", "imd_sim_exact_1"),
    annotations = idata$annotations |>
      dplyr::mutate(imd_sim_exact_1 = 0L)
  )

  expect_error(
    mutate_immundata(
      collision_idata,
      seq_options = make_seq_options(
        query_col = "cdr3_aa",
        patterns = "AAA",
        method = "exact"
      )
    ),
    "schema columns.*imd_sim_exact_1"
  )
})

test_that("mutate_immundata preserves repertoires, repertoire schema, and provenance", {
  idata <- make_mutate_test_idata() |>
    agg_repertoires("sample_id")

  reps_before <- idata$repertoires
  prov_before <- imd_get_provenance(idata)

  out <- mutate_immundata(idata, cohort = "all")

  expect_equal(out$repertoires, reps_before)
  expect_equal(out$schema_repertoire, idata$schema_repertoire)
  expect_equal(
    imd_get_provenance(out)[sort(names(imd_get_provenance(out)))],
    prov_before[sort(names(prov_before))]
  )
})

test_that("mutate_immundata supports exact sequence annotations", {
  idata <- make_mutate_test_idata()

  out <- mutate_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = c("AAA", "BBB"),
      method = "exact"
    )
  )

  ann <- out$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_true(all(c("imd_sim_exact_1", "imd_sim_exact_2") %in% names(ann)))
  expect_equal(ann$imd_sim_exact_1, ann$cdr3_aa == "AAA")
  expect_equal(ann$imd_sim_exact_2, ann$cdr3_aa == "BBB")
})

test_that("mutate_immundata supports pattern-based sequence annotation names", {
  idata <- make_mutate_test_idata()

  out <- mutate_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = "A-A",
      method = "exact",
      name_type = "pattern"
    )
  )

  expect_true("imd_sim_exact_A_A" %in% names(out$annotations))
})

test_that("mutate_immundata supports regex sequence annotations", {
  idata <- make_mutate_test_idata()

  out <- mutate_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = "^AA",
      method = "regex"
    )
  )

  ann <- out$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(ann$imd_sim_regex_1, grepl("^AA", ann$cdr3_aa))
})

test_that("mutate_immundata supports Levenshtein and Hamming distance annotations", {
  idata <- make_mutate_test_idata()

  lev <- mutate_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = "AAA",
      method = "lev"
    )
  )$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  hamm <- mutate_immundata(
    idata,
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = "AAA",
      method = "hamm"
    )
  )$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(lev$imd_sim_lev_1, c(0, 1, 1, 3))
  expect_equal(hamm$imd_sim_hamm_1, c(0, 1, NA, 3))
})

test_that("mutate_immundata validates seq_options", {
  idata <- make_mutate_test_idata()

  expect_error(
    mutate_immundata(idata, seq_options = list(patterns = "AAA")),
    "Missing fields"
  )
  expect_error(
    mutate_immundata(idata, seq_options = list(query_col = "cdr3_aa")),
    "Missing fields"
  )
})
