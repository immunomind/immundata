make_mutate_test_idata <- function() {
  ImmunData$new(
    schema = c("cdr3_aa", "v_call"),
    annotations = make_basic_test_annotations(),
    provenance = list(
      home_path = tempdir(),
      current_path = tempdir(),
      snapshot_id = "mutate-test-snapshot",
      lineage = list(list(event = "fixture"))
    )
  )
}

make_grouped_mutate_test_idata <- function() {
  annotations <- tibble::tibble(
    imd_receptor_id = c(1L, 1L, 2L, 3L, 4L),
    imd_barcode = paste0("bc", seq_len(5L)),
    imd_chain_id = seq_len(5L),
    imd_n_chains = 1L,
    cdr3_aa = c("AAA", "AAA", "BBB", "CCC", "DDD"),
    group = c("A", "A", "A", "B", "B"),
    batch = c("x", "x", "y", "x", "x"),
    value = c(1, 3, 5, 10, 14)
  ) |>
    duckplyr::as_duckdb_tibble(prudence = "stingy")

  ImmunData$new(
    schema = "cdr3_aa",
    annotations = annotations
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

test_that("grouped mutate forwards .by without creating a .by column", {
  idata <- make_grouped_mutate_test_idata()

  out <- idata |>
    mutate(
      centered = value - mean(value, na.rm = TRUE),
      above_mean = value > mean(value, na.rm = TRUE),
      .by = group
    )

  expect_false(".by" %in% names(out$annotations))
  expect_s3_class(out$annotations, "prudent_duckplyr_df")

  ann <- out |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(ann$centered, c(-2, 0, 2, -2, 2))
  expect_equal(ann$above_mean, c(FALSE, FALSE, TRUE, FALSE, TRUE))
})

test_that("grouped mutate falls back once for independent group summaries", {
  idata <- make_grouped_mutate_test_idata()

  expect_error(
    idata$annotations |>
      mutate(group_n_receptors = n_distinct(imd_receptor_id), .by = group),
    "not supported in window functions",
    fixed = TRUE
  )

  out <- idata |>
    mutate(
      group_n_rows = n(),
      group_n_receptors = n_distinct(imd_receptor_id),
      group_max = max(value),
      .by = group
    )

  expect_s3_class(out$annotations, "prudent_duckplyr_df")
  expect_error(
    nrow(out$annotations),
    "Materialization is disabled",
    fixed = TRUE
  )

  ann <- out |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(ann$group_n_rows, c(3, 3, 3, 2, 2))
  expect_equal(ann$group_n_receptors, c(2, 2, 2, 2, 2))
  expect_equal(ann$group_max, c(5, 5, 5, 14, 14))
})

test_that("group summary fallback supports multiple and missing group values", {
  idata <- make_grouped_mutate_test_idata()

  multiple <- idata |>
    mutate(
      group_n_receptors = n_distinct(imd_receptor_id),
      .by = c(group, batch)
    ) |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(multiple$group_n_receptors, c(1, 1, 1, 2, 2))

  missing_groups <- ImmunData$new(
    schema = "cdr3_aa",
    annotations = tibble::tibble(
      imd_receptor_id = c(1L, 2L, 3L, 3L),
      imd_barcode = paste0("bc", seq_len(4L)),
      imd_chain_id = seq_len(4L),
      imd_n_chains = 1L,
      cdr3_aa = c("AAA", "BBB", "CCC", "CCC"),
      group = c("A", "A", NA, NA)
    ) |>
      duckplyr::as_duckdb_tibble(prudence = "stingy")
  ) |>
    mutate(
      group_n_receptors = n_distinct(imd_receptor_id),
      .by = group
    ) |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(missing_groups$group_n_receptors, c(2, 2, 1, 1))
})

test_that("group summary fallback can replace a non-protected column", {
  idata <- make_grouped_mutate_test_idata()

  out <- idata |>
    mutate(value = n_distinct(imd_receptor_id), .by = group)

  expect_equal(names(out$annotations), names(idata$annotations))

  ann <- out |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(ann$value, rep(2, 5))
})

test_that("group summary fallback does not hide unrelated errors", {
  idata <- make_grouped_mutate_test_idata()

  expect_error(
    idata |>
      mutate(result = no_such_function(value), .by = group),
    "Can't translate function `no_such_function()`.",
    fixed = TRUE
  )
  expect_error(
    idata |>
      mutate(result = absent + 1, .by = group),
    "object 'absent' not found",
    fixed = TRUE
  )
  expect_error(
    idata |>
      mutate(result = n(), .by = absent),
    "Column `absent` doesn't exist",
    fixed = TRUE
  )
})

test_that("mixed row and fallback calculations can be split across mutate calls", {
  idata <- make_grouped_mutate_test_idata()

  expect_error(
    idata |>
      mutate(
        centered = value - mean(value, na.rm = TRUE),
        group_n_receptors = n_distinct(imd_receptor_id),
        .by = group
      ),
    "not supported in window functions",
    fixed = TRUE
  )

  out <- idata |>
    mutate(
      centered = value - mean(value, na.rm = TRUE),
      .by = group
    ) |>
    mutate(
      group_n_receptors = n_distinct(imd_receptor_id),
      .by = group
    ) |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(out$centered, c(-2, 0, 2, -2, 2))
  expect_equal(out$group_n_receptors, rep(2, 5))
})

test_that("fallback summary dependencies can be split across mutate calls", {
  idata <- make_grouped_mutate_test_idata()

  expect_error(
    idata |>
      mutate(
        group_n_receptors = n_distinct(imd_receptor_id),
        twice_group_n_receptors = group_n_receptors * 2,
        .by = group
      ),
    "not supported in window functions",
    fixed = TRUE
  )

  out <- idata |>
    mutate(
      group_n_receptors = n_distinct(imd_receptor_id),
      .by = group
    ) |>
    mutate(twice_group_n_receptors = group_n_receptors * 2) |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(out$group_n_receptors, rep(2, 5))
  expect_equal(out$twice_group_n_receptors, rep(4, 5))
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

test_that("mutate_immundata preserves repertoire, strata, and provenance state", {
  idata <- make_mutate_test_idata() |>
    agg_repertoires("sample_id") |>
    agg_strata("sample_id")

  reps_before <- idata$repertoires
  strata_before <- idata$strata
  prov_before <- get_provenance(idata)
  annotation_state_before <- idata$annotations |>
    select(imd_receptor_id, imd_repertoire_id, imd_strata_id) |>
    collect() |>
    arrange(imd_receptor_id)

  out <- mutate_immundata(idata, cohort = "all")

  expect_equal(out$repertoires, reps_before)
  expect_equal(out$strata, strata_before)
  expect_equal(out$schema_repertoire, idata$schema_repertoire)
  expect_equal(out$schema_strata, idata$schema_strata)
  expect_equal(
    out$annotations |>
      select(imd_receptor_id, imd_repertoire_id, imd_strata_id) |>
      collect() |>
      arrange(imd_receptor_id),
    annotation_state_before
  )
  expect_equal(
    get_provenance(out)[sort(names(get_provenance(out)))],
    prov_before[sort(names(prov_before))]
  )
})

test_that("mutate_immundata supports repertoire-free and strata-free state", {
  annotations_only <- make_mutate_test_idata()
  annotations_only_out <- mutate_immundata(annotations_only, cohort = "all")

  expect_null(annotations_only_out$repertoires)
  expect_null(annotations_only_out$strata)
  expect_null(annotations_only_out$schema_repertoire)
  expect_null(annotations_only_out$schema_strata)

  repertoires_only <- annotations_only |>
    agg_repertoires("sample_id")
  repertoires_only_out <- mutate_immundata(repertoires_only, cohort = "all")

  expect_equal(repertoires_only_out$repertoires, repertoires_only$repertoires)
  expect_equal(
    repertoires_only_out$schema_repertoire,
    repertoires_only$schema_repertoire
  )
  expect_null(repertoires_only_out$strata)
  expect_null(repertoires_only_out$schema_strata)
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
