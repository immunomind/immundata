create_test_output_dir <- function(name = "test_immundata_") {
  tempfile(name)
}

cleanup_output_dir <- function(dir) {
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
}

snapshot_test_root <- function() {
  normalizePath(file.path(tempdir(), "imd_snap_tests"), mustWork = FALSE)
}

create_snapshot_test_layout <- function() {
  root <- snapshot_test_root()
  if (dir.exists(root)) {
    unlink(root, recursive = TRUE)
  }

  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  project_a <- file.path(root, "projectA")
  project_b <- file.path(root, "projectB")
  dir.create(project_a, recursive = TRUE, showWarnings = FALSE)
  dir.create(project_b, recursive = TRUE, showWarnings = FALSE)

  list(
    root = root,
    projectA = project_a,
    projectB = project_b
  )
}

cleanup_snapshot_test_root <- function() {
  keep_snap_tests <- identical(Sys.getenv("IMD_KEEP_SNAP_TESTS"), "1")
  if (keep_snap_tests) {
    return(invisible(NULL))
  }

  root <- snapshot_test_root()
  if (dir.exists(root)) {
    unlink(root, recursive = TRUE)
  }
}

test_ig_data <- function() {
  system.file("extdata/ig", "multiple_ig_loci.tsv.gz", package = "immundata")
}

get_test_ig_data <- function(
    mode = c("single", "paired", "relaxed"),
    features = c("v_call", "j_call", "junction_aa"),
    output_folder = create_test_output_dir(),
    preprocess = NULL,
    postprocess = NULL,
    rename_columns = NULL) {
  if (is.null(preprocess)) {
    immundata:::get_test_ig_data(
      mode = mode,
      features = features,
      output_folder = output_folder,
      postprocess = postprocess,
      rename_columns = rename_columns
    )
  } else {
    immundata:::get_test_ig_data(
      mode = mode,
      features = features,
      output_folder = output_folder,
      preprocess = preprocess,
      postprocess = postprocess,
      rename_columns = rename_columns
    )
  }
}

load_test_ig_idata <- get_test_ig_data

format_integrity_df_dump <- function(df) {
  if (is.null(df)) {
    return("<NULL>")
  }

  old_width <- getOption("width")
  old_max_print <- getOption("max.print")
  on.exit(options(width = old_width, max.print = old_max_print), add = TRUE)
  options(width = 10000, max.print = 1e6)

  data_dump <- as.data.frame(df, stringsAsFactors = FALSE)
  dump <- utils::capture.output(print(data_dump, row.names = FALSE, right = FALSE))
  if (length(dump) == 0) {
    return("<no output>")
  }
  paste(dump, collapse = "\n")
}

expect_agg_repertoires_integrity <- function(
    idata,
    context = NULL,
    before_annotations = NULL,
    before_repertoires = NULL,
    schema = NULL
) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_data_frame(before_annotations, null.ok = TRUE)
  checkmate::assert_data_frame(before_repertoires, null.ok = TRUE)
  checkmate::assert_character(schema, null.ok = TRUE)

  if (is.null(context)) {
    context <- "agg_repertoires integrity"
  }

  ann <- idata$annotations |> collect()
  reps <- idata$repertoires |> collect()

  ann_required <- c("imd_receptor_id", "imd_repertoire_id", "imd_count", "imd_proportion", "n_repertoires")
  reps_required <- c("imd_repertoire_id", "n_barcodes", "n_receptors")

  ann_missing <- setdiff(ann_required, names(ann))
  reps_missing <- setdiff(reps_required, names(reps))

  ann_na_cols <- intersect(ann_required, names(ann))
  reps_na_cols <- intersect(reps_required, names(reps))

  ann_na_counts <- if (length(ann_na_cols) > 0) {
    vapply(ann[ann_na_cols], function(x) sum(is.na(x)), integer(1))
  } else {
    integer()
  }

  reps_na_counts <- if (length(reps_na_cols) > 0) {
    vapply(reps[reps_na_cols], function(x) sum(is.na(x)), integer(1))
  } else {
    integer()
  }

  unmatched_repertoire_ids <- NULL
  unmatched_repertoire_ids_display <- "<not checked>"
  if ("imd_repertoire_id" %in% names(ann) && "imd_repertoire_id" %in% names(reps)) {
    unmatched_repertoire_ids <- ann |>
      dplyr::distinct(imd_repertoire_id) |>
      dplyr::anti_join(
        reps |> dplyr::distinct(imd_repertoire_id),
        by = "imd_repertoire_id"
      )

    unmatched_repertoire_ids_display <- if (nrow(unmatched_repertoire_ids) > 0) {
      paste(unmatched_repertoire_ids$imd_repertoire_id, collapse = ", ")
    } else {
      "<none>"
    }
  }

  mapping_mismatches <- NULL
  mapping_mismatches_display <- "<not checked>"
  if (!is.null(schema)) {
    mapping_cols <- unique(c("imd_repertoire_id", schema))

    if (all(mapping_cols %in% names(ann)) && all(mapping_cols %in% names(reps))) {
      annotation_map <- ann |>
        dplyr::distinct(dplyr::across(dplyr::all_of(mapping_cols)))

      repertoire_map <- reps |>
        dplyr::distinct(dplyr::across(dplyr::all_of(mapping_cols)))

      mapping_mismatches <- dplyr::bind_rows(
        annotation_map |>
          dplyr::anti_join(repertoire_map, by = mapping_cols) |>
          dplyr::mutate(.mapping_source = "annotations"),
        repertoire_map |>
          dplyr::anti_join(annotation_map, by = mapping_cols) |>
          dplyr::mutate(.mapping_source = "repertoires")
      )

      mapping_mismatches_display <- as.character(nrow(mapping_mismatches))
    }
  }

  diag_lines <- c(
    paste0("context: ", context),
    if (!is.null(schema)) {
      paste0("agg schema: ", paste(schema, collapse = ", "))
    } else {
      "agg schema: <unknown>"
    },
    if (!is.null(before_annotations)) {
      paste0("input annotation shape: ", nrow(before_annotations), "x", ncol(before_annotations))
    } else {
      "input annotation shape: <not captured>"
    },
    if (!is.null(before_repertoires)) {
      paste0("input repertoire shape: ", nrow(before_repertoires), "x", ncol(before_repertoires))
    } else {
      "input repertoire shape: <not captured>"
    },
    paste0("annotation shape: ", nrow(ann), "x", ncol(ann)),
    paste0("repertoire shape: ", nrow(reps), "x", ncol(reps)),
    if (length(ann_missing) > 0) {
      paste0("missing annotation columns: ", paste(ann_missing, collapse = ", "))
    } else {
      "missing annotation columns: <none>"
    },
    if (length(reps_missing) > 0) {
      paste0("missing repertoire columns: ", paste(reps_missing, collapse = ", "))
    } else {
      "missing repertoire columns: <none>"
    },
    if (length(ann_na_counts) > 0) {
      paste0("annotation NA counts: ", paste(names(ann_na_counts), ann_na_counts, sep = "=", collapse = ", "))
    } else {
      "annotation NA counts: <none>"
    },
    if (length(reps_na_counts) > 0) {
      paste0("repertoire NA counts: ", paste(names(reps_na_counts), reps_na_counts, sep = "=", collapse = ", "))
    } else {
      "repertoire NA counts: <none>"
    },
    paste0("unmatched repertoire ids: ", unmatched_repertoire_ids_display),
    paste0("repertoire mapping mismatch rows: ", mapping_mismatches_display)
  )
  diag <- paste(diag_lines, collapse = "\n")

  has_ann_na_mismatch <- length(ann_na_counts) > 0 && any(ann_na_counts != 0)
  has_reps_na_mismatch <- length(reps_na_counts) > 0 && any(reps_na_counts != 0)
  has_unmatched_repertoire_ids <- !is.null(unmatched_repertoire_ids) && nrow(unmatched_repertoire_ids) > 0
  has_mismatch <- nrow(reps) <= 0 ||
    length(ann_missing) > 0 ||
    length(reps_missing) > 0 ||
    has_ann_na_mismatch ||
    has_reps_na_mismatch ||
    has_unmatched_repertoire_ids

  if (has_mismatch) {
    before_input_dump <- if (!is.null(before_annotations) || !is.null(before_repertoires)) {
      paste0(
        "\n\ninput annotations dump:\n",
        format_integrity_df_dump(before_annotations),
        "\n\ninput repertoires dump:\n",
        format_integrity_df_dump(before_repertoires)
      )
    } else {
      ""
    }

    diag <- paste0(
      diag,
      before_input_dump,
      "\n\nannotations dump:\n",
      format_integrity_df_dump(ann),
      "\n\nrepertoires dump:\n",
      format_integrity_df_dump(reps)
    )
  }

  testthat::expect_true(nrow(reps) > 0, info = diag)
  testthat::expect_equal(length(ann_missing), 0, info = diag)
  testthat::expect_equal(length(reps_missing), 0, info = diag)

  if (length(ann_na_counts) > 0) {
    testthat::expect_true(all(ann_na_counts == 0), info = diag)
  }
  if (length(reps_na_counts) > 0) {
    testthat::expect_true(all(reps_na_counts == 0), info = diag)
  }

  if (!is.null(unmatched_repertoire_ids)) {
    testthat::expect_equal(
      nrow(unmatched_repertoire_ids),
      0,
      info = diag
    )
  }

  if (!is.null(mapping_mismatches)) {
    mapping_mismatches_preview <- utils::head(mapping_mismatches, 20L)
    mapping_diag <- paste0(
      diag,
      "\n\nrepertoire mapping mismatches:\n",
      format_integrity_df_dump(mapping_mismatches_preview),
      if (nrow(mapping_mismatches) > nrow(mapping_mismatches_preview)) {
        paste0(
          "\n... ",
          nrow(mapping_mismatches) - nrow(mapping_mismatches_preview),
          " additional mismatch rows omitted"
        )
      } else {
        ""
      }
    )

    testthat::expect_equal(
      nrow(mapping_mismatches),
      0L,
      info = mapping_diag
    )
  }

  invisible(list(
    annotations = ann,
    repertoires = reps
  ))
}

agg_repertoires_with_integrity <- function(idata, schema, context = NULL) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(schema, min.len = 1)
  checkmate::assert_character(context, null.ok = TRUE)

  input_annotations <- idata$annotations |> collect()
  input_repertoires <- if (is.null(idata$repertoires)) {
    NULL
  } else {
    idata$repertoires |> collect()
  }

  idata_agg <- agg_repertoires(idata, schema = schema)

  expect_agg_repertoires_integrity(
    idata = idata_agg,
    context = context,
    before_annotations = input_annotations,
    before_repertoires = input_repertoires,
    schema = schema
  )

  idata_agg
}
