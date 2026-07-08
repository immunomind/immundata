test_that("agg_strata adds id and name to repertoires, id only to annotations", {
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy"))

  stratified <- agg_strata(idata, by = "Response")
  rep_tbl <- stratified$repertoires

  expect_s3_class(rep_tbl, "tbl_df")
  expect_true(strata_col %in% names(rep_tbl))
  expect_true(strata_name_col %in% names(rep_tbl))
  expect_true(strata_col %in% names(stratified$annotations))
  expect_false(strata_name_col %in% names(stratified$annotations))

  expected_n <- length(unique(rep_tbl$Response))
  observed_n <- length(unique(rep_tbl[[strata_col]]))
  expect_equal(observed_n, expected_n)

  rep_names <- unique(rep_tbl[c(strata_col, strata_name_col)])
  expect_equal(anyDuplicated(rep_names[[strata_col]]), 0)
  expect_equal(anyDuplicated(rep_names[[strata_name_col]]), 0)
})

test_that("agg_strata validates grouping columns", {
  idata <- get_test_immundata() |>
    agg_repertoires("Response")

  expect_error(
    agg_strata(idata, by = "not_a_metadata_column"),
    "not found in idata\\$repertoires"
  )
})

test_that("agg_strata keeps repertoire-strata mapping consistent", {
  repertoire_col <- imd_schema("repertoire")
  strata_col <- imd_schema("strata")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy"))

  stratified <- agg_strata(idata, by = c("Response", "Therapy"))

  rep_map <- unique(stratified$repertoires[c(repertoire_col, strata_col)])
  rep_map <- rep_map[order(rep_map[[repertoire_col]]), , drop = FALSE]

  ann_map <- stratified$annotations |>
    dplyr::select(all_of(c(repertoire_col, strata_col))) |>
    dplyr::collect()
  ann_map <- unique(ann_map[c(repertoire_col, strata_col)])
  ann_map <- ann_map[order(ann_map[[repertoire_col]]), , drop = FALSE]

  expect_equal(nrow(rep_map), nrow(ann_map))
  expect_equal(anyDuplicated(rep_map[[repertoire_col]]), 0)
  expect_equal(anyDuplicated(ann_map[[repertoire_col]]), 0)
  expect_equal(
    as.data.frame(rep_map, stringsAsFactors = FALSE),
    as.data.frame(ann_map, stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
})

test_that("agg_strata handles NA group values", {
  repertoire_col <- imd_schema("repertoire")
  strata_col <- imd_schema("strata")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy"))

  rep_tbl <- idata$repertoires
  rep_tbl$Response[1] <- NA_character_

  idata_with_na <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = idata$annotations,
    repertoires = rep_tbl
  )

  stratified <- agg_strata(idata_with_na, by = "Response")
  strat_rep <- stratified$repertoires

  expect_equal(
    length(unique(strat_rep[[strata_col]])),
    length(unique(strat_rep$Response))
  )

  na_rep <- unique(strat_rep[is.na(strat_rep$Response), c(repertoire_col, strata_col), drop = FALSE])
  expect_equal(nrow(na_rep), 1)

  ann_na_strata <- stratified$annotations |>
    dplyr::select(all_of(c(repertoire_col, strata_col))) |>
    dplyr::collect()
  ann_na_strata <- unique(
    ann_na_strata[ann_na_strata[[repertoire_col]] == na_rep[[repertoire_col]], strata_col, drop = FALSE]
  )

  expect_equal(nrow(ann_na_strata), 1)
  expect_equal(ann_na_strata[[strata_col]], na_rep[[strata_col]])
})

test_that("agg_strata requires repertoire aggregation", {
  idata <- get_test_immundata()

  expect_error(
    agg_strata(idata, by = "Response"),
    "agg_repertoires"
  )
})

test_that("agg_strata keeps repertoire schema and strata metadata are dropped on re-aggregation", {
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy"))

  stratified <- agg_strata(idata, by = "Response")

  expect_equal(stratified$schema_repertoire, idata$schema_repertoire)
  expect_true(strata_col %in% names(stratified$annotations))
  expect_true(strata_col %in% names(stratified$repertoires))
  expect_true(strata_name_col %in% names(stratified$repertoires))

  reaggregated <- agg_repertoires(stratified, schema = stratified$schema_repertoire)

  expect_false(strata_col %in% names(reaggregated$annotations))
  expect_false(strata_name_col %in% names(reaggregated$annotations))
  expect_false(strata_col %in% names(reaggregated$repertoires))
  expect_false(strata_name_col %in% names(reaggregated$repertoires))
})

test_that("re-aggregation rebuilds repertoire ids and removes strata metadata", {
  repertoire_col <- imd_schema("repertoire")
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy")) |>
    agg_strata(by = "Response")

  poisoned_annotations <- idata$annotations |>
    dplyr::mutate(!!repertoire_col := 999999L, !!strata_col := 999999L)

  poisoned_repertoires <- idata$repertoires
  poisoned_repertoires[[repertoire_col]] <- 999999L
  poisoned_repertoires[[strata_col]] <- 999999L
  poisoned_repertoires[[strata_name_col]] <- "BrokenName"

  poisoned_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = poisoned_annotations,
    repertoires = poisoned_repertoires
  )

  reaggregated <- agg_repertoires(poisoned_idata, schema = poisoned_idata$schema_repertoire)

  expect_false(strata_col %in% names(reaggregated$annotations))
  expect_false(strata_name_col %in% names(reaggregated$annotations))
  expect_false(strata_col %in% names(reaggregated$repertoires))
  expect_false(strata_name_col %in% names(reaggregated$repertoires))

  expect_true(repertoire_col %in% names(reaggregated$annotations))
  expect_true(repertoire_col %in% names(reaggregated$repertoires))

  ann_tbl <- reaggregated$annotations |>
    dplyr::select(all_of(repertoire_col)) |>
    dplyr::collect()
  rep_tbl <- reaggregated$repertoires

  expect_false(any(ann_tbl[[repertoire_col]] == 999999L))
  expect_false(any(rep_tbl[[repertoire_col]] == 999999L))
  expect_equal(length(unique(rep_tbl[[repertoire_col]])), nrow(rep_tbl))
  expect_true(all(ann_tbl[[repertoire_col]] %in% rep_tbl[[repertoire_col]]))
})

test_that("agg_strata can be re-run and overwrites previous strata assignment", {
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy"))

  rep_tbl <- idata$repertoires
  rep_tbl$Response <- "mixed"

  mutated <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = idata$annotations,
    repertoires = rep_tbl
  )

  stratified_once <- agg_strata(mutated, by = "Response")
  expect_equal(length(unique(stratified_once$repertoires[[strata_col]])), 1)

  stratified_twice <- agg_strata(stratified_once, by = "Therapy")
  expect_equal(length(unique(stratified_twice$repertoires[[strata_col]])), 2)
  expect_true(strata_name_col %in% names(stratified_twice$repertoires))
  expect_false(strata_name_col %in% names(stratified_twice$annotations))
})

test_that("agg_strata validates by argument contract", {
  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy"))

  expect_error(agg_strata(idata, by = character()), "Assertion on 'by' failed")
  expect_error(agg_strata(idata, by = NA_character_), "Assertion on 'by' failed")
  expect_error(agg_strata(idata, by = c("Response", "Response")), "Assertion on 'by' failed")
  expect_error(agg_strata(idata, by = 1), "Assertion on 'by' failed")
})

test_that("agg_strata errors clearly when repertoire id column is missing", {
  repertoire_col <- imd_schema("repertoire")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy"))

  bad_annotations <- idata$annotations |>
    dplyr::select(-all_of(repertoire_col))

  bad_ann_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = bad_annotations,
    repertoires = idata$repertoires
  )

  expect_error(
    agg_strata(bad_ann_idata, by = "Response"),
    "missing in .*idata\\$annotations"
  )

  bad_repertoires <- idata$repertoires
  bad_repertoires[[repertoire_col]] <- NULL

  bad_rep_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = idata$annotations,
    repertoires = bad_repertoires
  )

  expect_error(
    agg_strata(bad_rep_idata, by = "Response"),
    "missing in .*idata\\$repertoires"
  )
})

test_that("rename_strata renames using a full named vector mapping", {
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy")) |>
    agg_strata(by = "Response")

  strata_ids <- sort(unique(idata$repertoires[[strata_col]]))
  new_labels <- paste0("Group_", strata_ids)
  names(new_labels) <- as.character(strata_ids)

  renamed <- rename_strata(idata, names = new_labels)
  rep_tbl <- renamed$repertoires

  expect_equal(
    sort(unique(rep_tbl[[strata_name_col]])),
    sort(unname(new_labels))
  )
  expect_false(strata_name_col %in% names(renamed$annotations))
})

test_that("rename_strata supports partial renaming with unnamed policy", {
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy")) |>
    agg_strata(by = "Response")

  strata_ids <- sort(unique(idata$repertoires[[strata_col]]))
  partial_map <- c("CustomA")
  names(partial_map) <- as.character(strata_ids[1])

  expect_error(
    rename_strata(idata, names = partial_map),
    "Missing names for strata ID"
  )

  auto_named <- rename_strata(idata, names = partial_map, unnamed = "auto", auto_prefix = "Auto")
  rep_tbl <- auto_named$repertoires

  expect_true("CustomA" %in% rep_tbl[[strata_name_col]])
  expect_true(any(grepl("^Auto", rep_tbl[[strata_name_col]])))

  keep_named <- rename_strata(idata, names = partial_map, unnamed = "keep")
  keep_tbl <- keep_named$repertoires
  expect_true("CustomA" %in% keep_tbl[[strata_name_col]])
})

test_that("rename_strata validates mapping integrity", {
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  idata <- get_test_immundata() |>
    agg_repertoires(c("Response", "Therapy")) |>
    agg_strata(by = "Response")

  strata_ids <- sort(unique(idata$repertoires[[strata_col]]))

  bad_dup_label <- data.frame(
    imd_strata_id = strata_ids,
    strata_name = rep("dup", length(strata_ids))
  )
  expect_error(rename_strata(idata, names = bad_dup_label), "duplicated strata labels")

  bad_unknown_id <- data.frame(
    imd_strata_id = 999999L,
    strata_name = "Unknown"
  )
  expect_error(rename_strata(idata, names = bad_unknown_id), "Unknown strata ID")

  bad_empty <- data.frame(
    imd_strata_id = strata_ids[1],
    strata_name = ""
  )
  expect_error(rename_strata(idata, names = bad_empty), "non-empty strings")

  rep_tbl <- rename_strata(idata, names = setNames("OkName", as.character(strata_ids[1])), unnamed = "auto")$repertoires
  expect_true(strata_name_col %in% names(rep_tbl))
})
