warn_if_pkg_missing <- function(pkg, test_context) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(
      sprintf(
        "Package '%s' is not installed; %s checks were not executed.",
        pkg,
        test_context
      ),
      call. = FALSE
    )
    return(TRUE)
  }

  FALSE
}

make_external_idata <- function() {
  idata <- get_test_idata_tsv_no_manifest()
  barcode_col <- imd_schema("barcode")

  barcode_tbl <- idata$annotations |>
    dplyr::select(barcode = !!rlang::sym(barcode_col)) |>
    dplyr::distinct(.data$barcode, .keep_all = TRUE) |>
    dplyr::collect() |>
    dplyr::slice_head(n = 2)

  ann <- data.frame(
    barcode = c(barcode_tbl$barcode[1], barcode_tbl$barcode[1], barcode_tbl$barcode[2]),
    external_score = c(1L, 999999L, 2L),
    stringsAsFactors = FALSE
  )

  idata_annotated <- annotate_barcodes(idata, annotations = ann, annot_col = "barcode")

  first_scores <- idata_annotated$annotations |>
    dplyr::select(barcode = !!rlang::sym(barcode_col), external_score) |>
    dplyr::collect() |>
    as.data.frame()

  first_score <- first_scores$external_score[
    match(barcode_tbl$barcode[1], first_scores$barcode)
  ]

  list(
    idata = idata_annotated,
    barcode_col = barcode_col,
    first_barcode = barcode_tbl$barcode[1],
    first_score = first_score
  )
}

make_mock_adata <- function(obs_names) {
  structure(
    list(
      obs_names = obs_names,
      obs = data.frame(
        placeholder = seq_along(obs_names),
        row.names = paste0("row_", seq_along(obs_names))
      )
    ),
    class = "AbstractAnnData"
  )
}

testthat::test_that("annotate_seurat transfers selected columns by barcode", {
  if (warn_if_pkg_missing("Seurat", "annotate_seurat")) {
    testthat::succeed()
    return(invisible(NULL))
  }

  idata <- get_test_idata_tsv_no_manifest()
  barcode_col <- imd_schema("barcode")

  ann <- idata$annotations |>
    dplyr::select(barcode = !!rlang::sym(barcode_col), cdr3_aa) |>
    dplyr::distinct(.data$barcode, .keep_all = TRUE) |>
    dplyr::collect()

  cells <- c(ann$barcode[1:3], "barcode_not_found")
  counts <- matrix(
    c(1, 0, 2, 3, 4, 5, 6, 7),
    nrow = 2,
    dimnames = list(c("geneA", "geneB"), cells)
  )
  sdata <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts))

  out <- annotate_seurat(idata, sdata, cols = "cdr3_aa")

  expected <- ann$cdr3_aa[match(cells, ann$barcode)]
  actual <- out@meta.data[cells, "cdr3_aa", drop = TRUE]

  testthat::expect_equal(unname(actual), expected)
})

testthat::test_that("annotate_seurat keeps first record per duplicated barcode", {
  if (warn_if_pkg_missing("Seurat", "annotate_seurat")) {
    testthat::succeed()
    return(invisible(NULL))
  }

  ext <- make_external_idata()

  counts <- matrix(
    c(1, 0, 2, 3, 4, 5),
    nrow = 2,
    dimnames = list(
      c("geneA", "geneB"),
      c(ext$first_barcode, "barcode_not_found_1", "barcode_not_found_2")
    )
  )
  sdata <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts))

  out <- annotate_seurat(ext$idata, sdata, cols = "external_score")

  testthat::expect_equal(
    as.integer(out@meta.data[ext$first_barcode, "external_score", drop = TRUE]),
    as.integer(ext$first_score)
  )
})

testthat::test_that("annotate_seurat errors for missing annotation columns", {
  if (warn_if_pkg_missing("Seurat", "annotate_seurat")) {
    testthat::succeed()
    return(invisible(NULL))
  }

  idata <- get_test_idata_tsv_no_manifest()

  counts <- matrix(
    c(1, 0, 2, 3, 4, 5),
    nrow = 2,
    dimnames = list(c("geneA", "geneB"), c("barcode_1", "barcode_2", "barcode_3"))
  )
  sdata <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts))

  testthat::expect_error(
    annotate_seurat(idata, sdata, cols = "column_does_not_exist"),
    "not found in idata\\$annotations"
  )
})

testthat::test_that("annotate_anndata transfers selected columns by obs_names", {
  ext <- make_external_idata()
  annotate_anndata <- immundata:::annotate_anndata

  adata <- make_mock_adata(c(ext$first_barcode, "barcode_not_found"))

  out <- annotate_anndata(ext$idata, adata, cols = "external_score")

  testthat::expect_equal(
    as.integer(out$obs$external_score[1]),
    as.integer(ext$first_score)
  )
  testthat::expect_true(is.na(out$obs$external_score[2]))
})

testthat::test_that("annotate_anndata validates obs_names and requested columns", {
  idata <- get_test_idata_tsv_no_manifest()
  annotate_anndata <- immundata:::annotate_anndata

  adata_empty <- structure(
    list(obs_names = character(0), obs = data.frame(placeholder = integer())),
    class = "AbstractAnnData"
  )
  testthat::expect_error(
    annotate_anndata(idata, adata_empty, cols = "cdr3_aa"),
    "missing or empty"
  )

  adata_bad_names <- structure(
    list(
      obs_names = c("cell_1", ""),
      obs = data.frame(placeholder = 1:2, row.names = c("r1", "r2"))
    ),
    class = "AbstractAnnData"
  )
  testthat::expect_error(
    annotate_anndata(idata, adata_bad_names, cols = "cdr3_aa"),
    "contains NA/empty values"
  )

  adata_dup <- structure(
    list(
      obs_names = c("cell_1", "cell_1"),
      obs = data.frame(placeholder = 1:2, row.names = c("r1", "r2"))
    ),
    class = "AbstractAnnData"
  )
  testthat::expect_error(
    annotate_anndata(idata, adata_dup, cols = "cdr3_aa"),
    "must be unique"
  )

  adata_ok <- make_mock_adata(c("cell_1", "cell_2"))
  testthat::expect_error(
    annotate_anndata(idata, adata_ok, cols = "column_does_not_exist"),
    "not found in idata\\$annotations"
  )
})
