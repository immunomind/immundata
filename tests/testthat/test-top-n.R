test_that("top_n selects distinct receptor and repertoire combinations", {
  idata <- get_test_idata()
  before <- idata$annotations |>
    dplyr::distinct(imd_receptor_id, imd_repertoire_id, imd_count) |>
    dplyr::collect()
  before_n <- nrow(idata$annotations |> dplyr::collect())

  top <- top_n(idata, order_by = "imd_count", n = 3L,
                          with_ties = FALSE)
  gc()
  selected <- top$annotations |>
    dplyr::distinct(imd_receptor_id, imd_repertoire_id) |>
    dplyr::collect()
  expected <- before |>
    dplyr::arrange(imd_repertoire_id, dplyr::desc(imd_count), imd_receptor_id) |>
    dplyr::slice_head(n = 3L, by = imd_repertoire_id) |>
    dplyr::select(imd_receptor_id, imd_repertoire_id) |>
    dplyr::arrange(imd_repertoire_id, imd_receptor_id)
  selected <- selected |>
    dplyr::arrange(imd_repertoire_id, imd_receptor_id)
  expect_s3_class(top, "ImmunData")
  expect_equal(selected, expected)
  expect_lte(nrow(selected), 3L * dplyr::n_distinct(before$imd_repertoire_id))
  expect_equal(nrow(idata$annotations |> dplyr::collect()), before_n)

  tied <- top_n(idata, order_by = "imd_count", n = 1L,
                           with_ties = TRUE)
  expect_true(nrow(tied$annotations |> dplyr::collect()) > 0)
  none <- top_n(idata, order_by = "imd_count", n = 0L)
  expect_equal(nrow(none$annotations |> dplyr::collect()), 0L)
})

test_that("top_n keeps paired cells and rebuilds repertoire statistics", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))

  paired_rows <- function(sample, cells, heavy, light) {
    data.frame(
      sample_id = sample,
      cell_id = rep(cells, each = 2),
      v_call = rep(c("IGHV1", "IGLV1"), length(cells)),
      j_call = rep(c("IGHJ1", "IGLJ1"), length(cells)),
      junction_aa = as.vector(rbind(heavy, light)),
      locus = rep(c("IGH", "IGL"), length(cells)),
      umi_count = rep(c(10L, 8L), length(cells))
    )
  }
  rows_a <- paired_rows("A", c("a1", "a2", "a3", "a4"),
                        c("CARW", "CARW", "CARW", "CBRW"),
                        c("CASL", "CASL", "CASL", "CBSL"))
  rows_b <- paired_rows("B", c("b1", "b2", "b3"),
                        c("CARW", "CDRW", "CDRW"),
                        c("CASL", "CDSL", "CDSL"))
  paths <- c(tempfile(fileext = ".tsv"), tempfile(fileext = ".tsv"))
  readr::write_tsv(rows_a, paths[1])
  readr::write_tsv(rows_b, paths[2])
  on.exit(unlink(paths), add = TRUE)
  idata <- read_repertoires(
    path = paths,
    schema = make_receptor_schema(c("v_call", "j_call", "junction_aa"),
                                  chains = c("IGH", "IGL")),
    barcode_col = "cell_id", locus_col = "locus", umi_col = "umi_count",
    repertoire_schema = "sample_id", output_folder = output_dir,
    preprocess = NULL, postprocess = NULL, rename_columns = NULL, verbose = FALSE
  )
  original <- idata$annotations |> dplyr::collect()

  top <- top_n(idata, order_by = "imd_count", n = 1L,
                          with_ties = FALSE)
  result <- top$annotations |> dplyr::collect()
  expect_equal(idata$annotations |> dplyr::collect(), original)
  expect_setequal(unique(result$sample_id), c("A", "B"))
  expect_equal(nrow(result), 10L)
  expect_equal(dplyr::n_distinct(result$imd_barcode), 5L)
  expect_true(all(result$imd_count[result$sample_id == "A"] == 3L))
  expect_true(all(result$imd_count[result$sample_id == "B"] == 2L))
  expect_true(all(result$imd_proportion == 1))
  expect_equal(top$repertoires |> dplyr::collect() |> dplyr::arrange(sample_id) |>
                 dplyr::pull(n_barcodes), c(3L, 2L))
  expect_equal(top$repertoires |> dplyr::collect() |> dplyr::arrange(sample_id) |>
                 dplyr::pull(n_receptors), c(1L, 1L))
  expect_equal(result |> dplyr::count(sample_id, locus) |> dplyr::pull(n),
               rep(c(3L, 2L), each = 2))

  ungrouped_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(ungrouped_dir), add = TRUE)
  ungrouped <- read_repertoires(
    path = paths,
    schema = make_receptor_schema(c("v_call", "j_call", "junction_aa"),
                                  chains = c("IGH", "IGL")),
    barcode_col = "cell_id", locus_col = "locus", umi_col = "umi_count",
    repertoire_schema = NULL, output_folder = ungrouped_dir,
    preprocess = NULL, postprocess = NULL, rename_columns = NULL, verbose = FALSE
  )
  global_top <- top_n(ungrouped)
  global_rows <- dplyr::collect(global_top)
  expect_equal(nrow(global_rows), 8L)
  expect_setequal(global_rows$cell_id, c("a1", "a2", "a3", "b1"))
  expect_setequal(global_rows$locus, c("IGH", "IGL"))
  expect_null(global_top$schema_repertoire)
  expect_null(global_top$repertoires)
})

test_that("top_n preserves bulk abundance rather than recounting rows", {
  output_dir <- create_test_output_dir()
  on.exit(cleanup_output_dir(output_dir))
  rows_a <- data.frame(sample_id = "A", v_call = "IGHV1", j_call = "IGHJ1",
                       junction_aa = c("CARW", "CBRW"), abundance = c(7L, 2L))
  rows_b <- data.frame(sample_id = "B", v_call = "IGHV1", j_call = "IGHJ1",
                       junction_aa = c("CARW", "CBRW"), abundance = c(1L, 6L))
  paths <- c(tempfile(fileext = ".tsv"), tempfile(fileext = ".tsv"))
  readr::write_tsv(rows_a, paths[1])
  readr::write_tsv(rows_b, paths[2])
  on.exit(unlink(paths), add = TRUE)
  idata <- read_repertoires(
    path = paths,
    schema = c("v_call", "j_call", "junction_aa"),
    count_col = "abundance", repertoire_schema = "sample_id",
    output_folder = output_dir, preprocess = NULL, postprocess = NULL,
    rename_columns = NULL, verbose = FALSE
  )

  top <- top_n(idata, order_by = "imd_count", n = 1L,
                          with_ties = FALSE)
  result <- top$annotations |>
    dplyr::distinct(sample_id, junction_aa, imd_count) |>
    dplyr::collect() |>
    dplyr::arrange(sample_id)
  expect_equal(result$sample_id, c("A", "B"))
  expect_equal(result$junction_aa, c("CARW", "CBRW"))
  expect_equal(result$imd_count, c(7L, 6L))
})

test_that("top_n handles tied cutoffs and preserves existing annotations", {
  output_dir <- create_test_output_dir()
  path <- tempfile(fileext = ".tsv")
  on.exit(cleanup_output_dir(output_dir))
  on.exit(unlink(path), add = TRUE)
  readr::write_tsv(data.frame(
    sample_id = c("A", "A", "A", "B", "B"),
    junction_aa = c("AAA", "BBB", "CCC", "AAA", "DDD"),
    abundance = c(9L, 5L, 5L, 9L, 1L),
    .imd_top_selected = letters[1:5],
    .imd_top_selected_ = LETTERS[1:5]
  ), path)
  idata <- read_repertoires(
    path = path, schema = "junction_aa", count_col = "abundance",
    repertoire_schema = "sample_id", output_folder = output_dir,
    preprocess = NULL, postprocess = NULL, rename_columns = NULL, verbose = FALSE
  )
  original <- dplyr::collect(idata)
  expect_s3_class(idata$annotations, "duckplyr_df")

  tied <- top_n(idata, n = 2L, with_ties = TRUE)
  tied_rows <- dplyr::collect(tied)
  expect_equal(nrow(tied_rows), 5L)
  expect_setequal(tied_rows$junction_aa[tied_rows$sample_id == "A"],
                  c("AAA", "BBB", "CCC"))
  untied <- top_n(idata, n = 2L)
  untied_rows <- dplyr::collect(untied)
  expect_equal(nrow(untied_rows), 4L)
  expect_setequal(untied_rows$junction_aa[untied_rows$sample_id == "A"],
                  c("AAA", "BBB"))

  default_order <- top_n(idata)
  default_rows <- dplyr::collect(default_order) |> dplyr::arrange(sample_id)
  expect_equal(default_rows$sample_id, c("A", "B"))
  expect_true(all(default_rows$junction_aa == "AAA"))
  expect_true(all(default_rows$imd_count == 9))
  expect_true(all(default_rows$imd_proportion == 1))
  explicit_order <- top_n(idata, n = 1L, order_by = "imd_count")
  expect_equal(dplyr::collect(explicit_order) |> dplyr::arrange(sample_id), default_rows)

  prop <- top_n(idata, n = 1L, order_by = "imd_proportion")
  gc()
  prop_rows <- dplyr::collect(prop) |> dplyr::arrange(sample_id)
  expect_equal(prop_rows$sample_id, c("A", "B"))
  expect_true(all(prop_rows$imd_count == 9))
  expect_true(all(prop_rows$imd_proportion == 1))
  expect_equal(prop_rows$.imd_top_selected, c("a", "d"))
  expect_equal(prop_rows$.imd_top_selected_, c("A", "D"))
  expect_setequal(names(prop_rows), names(original))
  expect_s3_class(prop$annotations, "duckplyr_df")

  all_rows <- top_n(idata, n = 100L)
  expect_equal(nrow(dplyr::collect(all_rows)), 5L)
  expect_equal(dplyr::collect(idata), original)
  expect_error(top_n(idata, n = 1L, order_by = "abundance"), "order_by")
  expect_error(top_n(idata, n = 1.5))
})

test_that("top_n without repertoires uses total bulk abundance", {
  output_dir <- create_test_output_dir()
  path <- tempfile(fileext = ".tsv")
  on.exit(cleanup_output_dir(output_dir))
  on.exit(unlink(path), add = TRUE)
  readr::write_tsv(data.frame(
    junction_aa = c("AAA", "BBB", "AAA", "BBB", "BBB", "CCC"),
    abundance = c(9L, 1L, 9L, 1L, 1L, 17L)
  ), path)
  idata <- read_repertoires(
    path = path, schema = "junction_aa", count_col = "abundance",
    repertoire_schema = NULL, output_folder = output_dir,
    preprocess = NULL, postprocess = NULL, rename_columns = NULL, verbose = FALSE
  )
  original <- dplyr::collect(idata)

  top <- top_n(idata, n = 1L, with_ties = TRUE)
  gc()
  result <- dplyr::collect(top)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$junction_aa == "AAA"))
  expect_equal(sum(result$abundance), 18)
  expect_null(top$schema_repertoire)
  expect_null(top$repertoires)
  expect_s3_class(top$annotations, "duckplyr_df")

  prop <- top_n(idata, n = 1L, order_by = "imd_proportion")
  expect_true(all(dplyr::collect(prop)$junction_aa == "AAA"))
  two <- top_n(idata, n = 2L)
  expect_setequal(dplyr::collect(two)$junction_aa, c("AAA", "CCC"))
  none <- top_n(idata, n = 0L)
  expect_equal(nrow(dplyr::collect(none)), 0L)
  expect_null(none$repertoires)
  expect_equal(dplyr::collect(idata), original)
})
