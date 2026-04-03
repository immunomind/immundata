# ============================================================================
# STEPWISE TEST: Single-chain agg_repertoires internals
# ============================================================================

test_that("agg_repertoires single-chain stepwise materialized path is stable", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  test_data <- data.frame(
    cell_id = c("cell1", "cell2", "cell3", "cell4", "cell5"),
    sample_id = c("Sample1", "Sample1", "Sample1", "Sample2", "Sample2"),
    v_call = c("IGHV1", "IGHV1", "IGHV2", "IGHV3", "IGHV4"),
    j_call = c("IGHJ1", "IGHJ1", "IGHJ2", "IGHJ3", "IGHJ4"),
    junction_aa = c("CARW", "CARW", "CBRW", "CCRW", "CDRW"),
    locus = c("IGH", "IGH", "IGH", "IGH", "IGH"),
    umi_count = c(100, 150, 200, 250, 300)
  )

  temp_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(test_data, temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  idata <- read_repertoires(
    path = temp_file,
    schema = make_receptor_schema(
      features = c("v_call", "j_call", "junction_aa"),
      chains = "IGH"
    ),
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "umi_count",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL
  )

  schema <- "sample_id"
  receptor_id <- imd_schema("receptor")
  repertoire_id <- imd_schema("repertoire")
  prop_col <- imd_schema("proportion")
  imd_count_col <- imd_schema("count")
  barcode_col <- imd_schema("barcode")
  chain_count_col <- imd_schema("chain_count")
  n_receptors_col <- imd_schema("n_receptors")
  n_barcodes_col <- imd_schema("n_barcodes")
  n_repertoires_col <- imd_schema("n_repertoires")
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

  cols_to_drop <- c(
    repertoire_id,
    strata_col,
    strata_name_col,
    imd_count_col,
    prop_col,
    n_receptors_col,
    n_barcodes_col,
    n_repertoires_col
  )

  new_annotations <- idata$annotations |>
    dplyr::select(-dplyr::any_of(cols_to_drop))

  single_chain_annotations <- new_annotations |>
    dplyr::distinct(
      !!rlang::sym(receptor_id),
      !!rlang::sym(barcode_col),
      .keep_all = TRUE
    )

  repertoires_table <- single_chain_annotations |>
    dplyr::summarise(
      .by = dplyr::all_of(schema),
      n_barcodes = sum(!!rlang::sym(chain_count_col))
    ) |>
    dplyr::mutate(!!repertoire_id := dplyr::row_number()) |>
    dplyr::relocate(!!rlang::sym(repertoire_id))

  receptor_cells <- single_chain_annotations |>
    dplyr::summarise(
      .by = dplyr::all_of(c(schema, receptor_id)),
      !!imd_count_col := sum(!!rlang::sym(chain_count_col))
    )

  # Materialize one join side to avoid duckdb 1.5.0 lazy join corruption.
  receptor_props <- receptor_cells |>
    dplyr::left_join(
      repertoires_table |> dplyr::compute(),
      by = schema
    ) |>
    dplyr::mutate(
      !!prop_col := !!rlang::sym(imd_count_col) / n_barcodes
    ) |>
    dplyr::select(-n_barcodes)

  joined_annotations <- new_annotations |>
    dplyr::left_join(receptor_props, by = c(schema, receptor_id))

  unique_receptors <- joined_annotations |>
    dplyr::distinct(
      !!rlang::sym(receptor_id),
      !!rlang::sym(repertoire_id)
    )

  n_receptors_df <- unique_receptors |>
    dplyr::summarise(
      .by = dplyr::all_of(repertoire_id),
      n_receptors = dplyr::n()
    )

  repertoires_table <- repertoires_table |>
    dplyr::left_join(n_receptors_df, by = repertoire_id)

  repertoire_counts <- unique_receptors |>
    dplyr::summarise(
      .by = dplyr::all_of(receptor_id),
      n_repertoires = dplyr::n()
    )

  joined_annotations <- joined_annotations |>
    dplyr::left_join(repertoire_counts, by = receptor_id)

  ann_df <- dplyr::collect(new_annotations)
  single_df <- dplyr::collect(single_chain_annotations)
  reps_df <- dplyr::collect(repertoires_table)
  receptor_cells_df <- dplyr::collect(receptor_cells)
  receptor_props_df <- dplyr::collect(receptor_props)
  joined_df <- dplyr::collect(joined_annotations)

  diag <- paste0(
    "annotation shape: ", nrow(ann_df), "x", ncol(ann_df), "\n",
    "single-chain shape: ", nrow(single_df), "x", ncol(single_df), "\n",
    "repertoires shape: ", nrow(reps_df), "x", ncol(reps_df), "\n",
    "receptor_cells shape: ", nrow(receptor_cells_df), "x", ncol(receptor_cells_df), "\n",
    "receptor_props shape: ", nrow(receptor_props_df), "x", ncol(receptor_props_df), "\n",
    "joined shape: ", nrow(joined_df), "x", ncol(joined_df), "\n\n",
    "repertoires dump:\n", format_integrity_df_dump(reps_df), "\n\n",
    "receptor_props dump:\n", format_integrity_df_dump(receptor_props_df), "\n\n",
    "joined dump:\n", format_integrity_df_dump(joined_df)
  )

  testthat::expect_equal(nrow(ann_df), 5, info = diag)
  testthat::expect_equal(nrow(single_df), 5, info = diag)
  testthat::expect_equal(nrow(reps_df), 2, info = diag)
  testthat::expect_equal(nrow(receptor_cells_df), 4, info = diag)
  testthat::expect_equal(nrow(receptor_props_df), 4, info = diag)
  testthat::expect_equal(nrow(joined_df), 5, info = diag)

  testthat::expect_true(all(!is.na(reps_df[[repertoire_id]])), info = diag)
  testthat::expect_true(all(!is.na(reps_df[[n_barcodes_col]])), info = diag)
  testthat::expect_true(all(!is.na(reps_df[[n_receptors_col]])), info = diag)

  testthat::expect_true(all(!is.na(receptor_props_df[[schema]])), info = diag)
  testthat::expect_true(all(!is.na(receptor_props_df[[receptor_id]])), info = diag)
  testthat::expect_true(all(!is.na(receptor_props_df[[repertoire_id]])), info = diag)
  testthat::expect_true(all(!is.na(receptor_props_df[[imd_count_col]])), info = diag)
  testthat::expect_true(all(!is.na(receptor_props_df[[prop_col]])), info = diag)

  reps_by_sample <- reps_df |>
    dplyr::arrange(sample_id)
  testthat::expect_equal(reps_by_sample$n_barcodes, c(3, 2), info = diag)
  testthat::expect_equal(reps_by_sample$n_receptors, c(2, 2), info = diag)

  prop_values <- sort(unique(receptor_props_df[[prop_col]]))
  testthat::expect_equal(prop_values, c(1 / 3, 1 / 2, 2 / 3), tolerance = 1e-10, info = diag)

  testthat::expect_true(all(!is.na(joined_df[[repertoire_id]])), info = diag)
  testthat::expect_true(all(!is.na(joined_df[[imd_count_col]])), info = diag)
  testthat::expect_true(all(!is.na(joined_df[[prop_col]])), info = diag)
  testthat::expect_true(all(!is.na(joined_df[[n_repertoires_col]])), info = diag)

  idata_agg <- agg_repertoires_with_integrity(
    idata,
    schema = schema,
    context = "stepwise single-chain materialized path"
  )

  ann_expected <- joined_df |>
    dplyr::select(
      dplyr::all_of(
        c(
          "sample_id",
          "cell_id",
          receptor_id,
          repertoire_id,
          imd_count_col,
          prop_col,
          n_repertoires_col
        )
      )
    ) |>
    dplyr::arrange(sample_id, cell_id)

  ann_actual <- idata_agg$annotations |>
    dplyr::collect() |>
    dplyr::select(
      dplyr::all_of(
        c(
          "sample_id",
          "cell_id",
          receptor_id,
          repertoire_id,
          imd_count_col,
          prop_col,
          n_repertoires_col
        )
      )
    ) |>
    dplyr::arrange(sample_id, cell_id)

  testthat::expect_equal(ann_actual, ann_expected, tolerance = 1e-10, info = diag)
})
