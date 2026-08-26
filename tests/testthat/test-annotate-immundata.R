make_annotate_test_idata <- function() {
  ImmunData$new(
    schema = c("cdr3_aa", "v_call"),
    annotations = make_basic_test_annotations(),
    provenance = list(
      home_path = tempdir(),
      current_path = tempdir(),
      snapshot_id = "annotate-test-snapshot",
      lineage = list(list(event = "fixture"))
    )
  )
}

test_that("annotate_immundata left-joins annotations with renamed keys", {
  idata <- make_annotate_test_idata()
  ann <- tibble::tibble(
    external_v = c("V1", "V2"),
    receptor_family = c("alpha", "beta")
  )

  out <- annotate_immundata(
    idata,
    annotations = ann,
    by = c("v_call" = "external_v"),
    keep_repertoires = FALSE
  )

  expected <- idata$annotations |>
    collect() |>
    left_join(ann, by = join_by(v_call == external_v)) |>
    arrange(imd_receptor_id)

  actual <- out$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(actual, expected)
  expect_true(is.na(actual$receptor_family[actual$v_call == "V3"]))
})

test_that("annotate alias and annotate_immundata produce equivalent annotations", {
  idata <- make_annotate_test_idata()
  ann <- tibble::tibble(v_call = c("V1", "V2"), receptor_family = c("alpha", "beta"))

  direct <- annotate_immundata(
    idata,
    annotations = ann,
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )
  alias <- annotate(
    idata,
    annotations = ann,
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )

  expect_equal(
    direct$annotations |> collect() |> arrange(imd_receptor_id),
    alias$annotations |> collect() |> arrange(imd_receptor_id)
  )
})

test_that("annotate_immundata supports same-name and multi-column keys", {
  idata <- make_annotate_test_idata()

  same_name <- tibble::tibble(v_call = "V1", v_label = "same-name")
  same_name_out <- annotate_immundata(
    idata,
    annotations = same_name,
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(same_name_out$v_label, c("same-name", "same-name", NA, NA))

  pair_ann <- tibble::tibble(
    external_v = c("V1", "V1", "V2"),
    external_cdr3 = c("AAA", "AAT", "AAA"),
    pair_label = c("hit-1", "hit-2", "miss")
  )
  pair_out <- annotate_immundata(
    idata,
    annotations = pair_ann,
    by = c("v_call" = "external_v", "cdr3_aa" = "external_cdr3"),
    keep_repertoires = FALSE
  )$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(pair_out$pair_label, c("hit-1", "hit-2", NA, NA))
})

test_that("non-unique annotation keys violate the contract and expand rows", {
  idata <- make_annotate_test_idata()
  ann <- tibble::tibble(
    v_call = c("V1", "V1"),
    label = c("first", "second")
  )

  out <- annotate_immundata(
    idata,
    annotations = ann,
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )

  expected <- idata$annotations |>
    collect() |>
    left_join(ann, by = "v_call") |>
    arrange(imd_receptor_id, label)

  actual <- out$annotations |>
    collect() |>
    arrange(imd_receptor_id, label)

  expect_equal(nrow(actual), nrow(expected))
  expect_equal(actual, expected)
})

test_that("annotate_immundata preserves repertoire and strata state", {
  idata <- make_annotate_test_idata() |>
    agg_repertoires("sample_id") |>
    agg_strata("sample_id")

  ann <- tibble::tibble(
    v_call = c("V1", "V2", "V3"),
    receptor_family = c("alpha", "beta", "gamma")
  )

  reps_before <- idata$repertoires
  strata_before <- idata$strata
  annotation_state_cols <- c(
    "imd_receptor_id",
    "imd_repertoire_id",
    "imd_strata_id",
    "imd_count",
    "imd_proportion",
    "n_repertoires"
  )
  annotation_state_before <- idata$annotations |>
    select(all_of(annotation_state_cols)) |>
    collect() |>
    arrange(across(everything()))

  out <- annotate_immundata(
    idata,
    annotations = ann,
    by = c("v_call" = "v_call"),
    keep_repertoires = TRUE
  )

  expect_true("receptor_family" %in% names(out$annotations))
  expect_equal(out$repertoires, reps_before)
  expect_equal(out$strata, strata_before)
  expect_equal(out$schema_repertoire, idata$schema_repertoire)
  expect_equal(out$schema_strata, idata$schema_strata)
  expect_equal(
    out$annotations |>
      select(all_of(annotation_state_cols)) |>
      collect() |>
      arrange(across(everything())),
    annotation_state_before
  )
})

test_that("annotate_immundata drops all repertoire state when keep_repertoires is FALSE", {
  idata <- make_annotate_test_idata() |>
    agg_repertoires("sample_id") |>
    agg_strata("sample_id")

  out <- annotate_immundata(
    idata,
    annotations = tibble::tibble(v_call = "V1", receptor_family = "alpha"),
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )

  expect_null(out$repertoires)
  expect_null(out$strata)
  expect_null(out$schema_repertoire)
  expect_null(out$schema_strata)
  expect_length(
    intersect(
      colnames(out$annotations),
      c(
        "imd_repertoire_id",
        "imd_strata_id",
        "strata_name",
        "imd_count",
        "imd_proportion",
        "n_receptors",
        "n_barcodes",
        "n_repertoires"
      )
    ),
    0
  )
})

test_that("annotate_immundata preserves provenance", {
  idata <- make_annotate_test_idata()
  prov_before <- get_provenance(idata)

  out <- annotate_immundata(
    idata,
    annotations = tibble::tibble(v_call = "V1", receptor_family = "alpha"),
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )

  expect_equal(
    get_provenance(out)[sort(names(get_provenance(out)))],
    prov_before[sort(names(prov_before))]
  )
})

test_that("annotate_immundata validates join inputs", {
  idata <- make_annotate_test_idata()

  expect_error(
    annotate_immundata(
      idata,
      annotations = tibble::tibble(external_v = "V1", receptor_family = "alpha"),
      by = c("v_call" = "missing_col")
    ),
    "not found in annotations"
  )

  expect_error(
    annotate_immundata(
      idata,
      annotations = tibble::tibble(external_v = "V1", receptor_family = "alpha"),
      by = c("missing_col" = "external_v")
    ),
    "not found in ImmunData"
  )

  expect_error(
    annotate_immundata(
      idata,
      annotations = tibble::tibble(external_v = "V1", receptor_family = "alpha"),
      by = "external_v"
    ),
    "Assertion on 'by' failed"
  )
})

test_that("annotate_immundata allows absent schema-named annotation columns", {
  idata <- make_annotate_test_idata()

  out <- annotate_immundata(
    idata,
    annotations = tibble::tibble(
      v_call = c("V1", "V2", "V3"),
      imd_group_id = c("group-1", "group-2", "group-3"),
      j_gene = c("J1", "J2", "J3")
    ),
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(
    out$imd_group_id,
    c("group-1", "group-1", "group-2", "group-3")
  )
  expect_equal(out$j_gene, c("J1", "J1", "J2", "J3"))
})

test_that("annotate_immundata blocks annotation columns that already exist", {
  idata <- make_annotate_test_idata()

  expect_error(
    annotate_immundata(
      idata,
      annotations = tibble::tibble(external_v = "V1", cdr3_aa = "collision"),
      by = c("v_call" = "external_v")
    ),
    "collide"
  )
})

test_that("annotate_immundata can replace annotation columns that already exist", {
  idata <- annotate_immundata(
    make_annotate_test_idata(),
    annotations = tibble::tibble(
      v_call = c("V1", "V2", "V3"),
      imd_group_id = c("old-1", "old-2", "old-3")
    ),
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE
  )

  out <- annotate_immundata(
    idata,
    annotations = tibble::tibble(
      v_call = c("V1", "V2", "V3"),
      imd_group_id = c("new-1", "new-2", "new-3")
    ),
    by = c("v_call" = "v_call"),
    keep_repertoires = FALSE,
    conflicts = "replace"
  )$annotations |>
    collect() |>
    arrange(imd_receptor_id)

  expect_equal(
    out$imd_group_id,
    c("new-1", "new-1", "new-2", "new-3")
  )
})

test_that("annotate_immundata cannot replace protected state or schema columns", {
  idata <- make_annotate_test_idata() |>
    agg_repertoires("sample_id") |>
    agg_strata("sample_id")

  expect_error(
    annotate_immundata(
      idata,
      annotations = tibble::tibble(
        v_call = c("V1", "V2", "V3"),
        imd_count = c(10L, 20L, 30L)
      ),
      by = c("v_call" = "v_call"),
      conflicts = "replace"
    ),
    "protected ImmunData columns"
  )

  expect_error(
    annotate_immundata(
      idata,
      annotations = tibble::tibble(
        v_call = c("V1", "V2", "V3"),
        sample_id = c("new-1", "new-2", "new-3")
      ),
      by = c("v_call" = "v_call"),
      conflicts = "replace"
    ),
    "protected ImmunData columns"
  )
})

test_that("annotate_immundata enforces and can bypass the wide annotation guard", {
  idata <- make_annotate_test_idata()
  wide <- as.data.frame(
    as.list(stats::setNames(rep("x", 100), paste0("feature_", seq_len(100)))),
    stringsAsFactors = FALSE
  )
  wide$v_call <- "V1"

  expect_error(
    annotate_immundata(
      idata,
      annotations = wide,
      by = c("v_call" = "v_call")
    ),
    "you have been warned"
  )

  out <- annotate_immundata(
    idata,
    annotations = wide,
    by = c("v_call" = "v_call"),
    remove_limit = TRUE,
    keep_repertoires = FALSE
  )

  expect_true("feature_1" %in% names(out$annotations))
})

test_that("annotate_chains annotates by explicit chain identifier column", {
  idata <- make_annotate_test_idata()
  ann <- tibble::tibble(
    chain_id = c(1L, 3L),
    chain_label = c("chain-a", "chain-c")
  )

  out <- annotate_chains(
    idata,
    annotations = ann,
    annot_col = "chain_id",
    keep_repertoires = FALSE
  )

  expected <- idata$annotations |>
    collect() |>
    left_join(ann, by = join_by(imd_chain_id == chain_id)) |>
    arrange(imd_chain_id)

  actual <- out$annotations |>
    collect() |>
    arrange(imd_chain_id)

  expect_equal(actual, expected)
})
