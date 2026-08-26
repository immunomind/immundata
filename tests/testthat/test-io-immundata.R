test_that("read_immundata() upgrades legacy v1 metadata on the fly", {
  legacy_path <- create_test_output_dir("legacy_v1_")
  on.exit(cleanup_output_dir(legacy_path), add = TRUE)

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = legacy_path,
    preprocess = NULL,
    postprocess = NULL
  )

  legacy_metadata_v1 <- list(
    version = as.character(packageVersion("immundata")),
    receptor_schema = list(
      features = c("cdr3_aa", "v_call"),
      chains = "TCRB"
    ),
    repertoire_schema = "imd_filename"
  )
  jsonlite::write_json(
    legacy_metadata_v1,
    path = file.path(legacy_path, "metadata.json"),
    auto_unbox = TRUE,
    null = "null"
  )

  expect_warning(
    idata <- read_immundata(legacy_path, verbose = FALSE),
    "legacy v1 metadata"
  )

  checkmate::expect_r6(idata, classes = "ImmunData")
  expect_true(length(names(idata$annotations)) > 0)
  expect_equal(idata$schema_repertoire, "imd_filename")
  expect_false(is.null(idata$repertoires))

  prov <- get_provenance(idata)
  expect_equal(prov$current_path, normalizePath(legacy_path, mustWork = FALSE))
  expect_null(prov$snapshot_id)

  written <- write_immundata(idata, output_folder = legacy_path)
  expect_true(is.character(get_provenance(written)$snapshot_id))
})

test_that("ImmunData$provenance is read-only and matches helper output", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  expect_identical(idata$provenance, get_provenance(idata))

  expect_error(
    idata$provenance <- list(),
    "read-only"
  )
})

test_that("normalize_provenance applies canonical overrides and derives all paths", {
  old_home <- create_test_output_dir("old_provenance_home_")
  new_home <- create_test_output_dir("new_provenance_home_")
  dir.create(old_home, recursive = TRUE)
  dir.create(new_home, recursive = TRUE)
  on.exit(cleanup_output_dir(old_home), add = TRUE)
  on.exit(cleanup_output_dir(new_home), add = TRUE)

  current_path <- file.path(
    new_home,
    "snapshots",
    "baseline",
    "v003"
  )
  dir.create(current_path, recursive = TRUE)
  canonical_lineage <- list(list(
    event = "snapshot",
    snapshot_id = "canonical-id"
  ))
  provenance <- normalize_provenance(
    provenance = list(
      home_path = old_home,
      current_path = old_home,
      snapshot_root = file.path(old_home, "stale-snapshots"),
      artifacts_root = file.path(old_home, "stale-artifacts"),
      artifacts_path = file.path(old_home, "stale-artifact-path"),
      snapshot_id = "stale-id",
      lineage = list(list(event = "stale"))
    ),
    home_path = new_home,
    current_path = current_path,
    snapshot_id = "canonical-id",
    lineage = canonical_lineage
  )

  normalized_home <- normalizePath(new_home, mustWork = TRUE)
  expect_equal(provenance$home_path, normalized_home)
  expect_equal(
    provenance$current_path,
    normalizePath(
      file.path(normalized_home, "snapshots", "baseline", "v003"),
      mustWork = FALSE
    )
  )
  expect_equal(
    provenance$snapshot_root,
    normalizePath(
      file.path(normalized_home, "snapshots"),
      mustWork = FALSE
    )
  )
  expect_equal(
    provenance$artifacts_root,
    normalizePath(
      file.path(normalized_home, "artifacts"),
      mustWork = FALSE
    )
  )
  expect_equal(
    provenance$artifacts_path,
    normalizePath(
      file.path(normalized_home, "artifacts", "baseline", "v003"),
      mustWork = FALSE
    )
  )
  expect_equal(provenance$snapshot_id, "canonical-id")
  expect_identical(provenance$lineage, canonical_lineage)

  metadata_provenance <- provenance_paths_for_metadata(provenance)
  expect_named(
    metadata_provenance,
    c(
      "home_path", "current_path", "snapshot_root",
      "artifacts_root", "artifacts_path"
    )
  )
  expect_false("snapshot_id" %in% names(metadata_provenance))
  expect_false("lineage" %in% names(metadata_provenance))
})

test_that("root ingestion exposes a shared artifacts root and root artifact path", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  normalized_output <- normalizePath(output_dir, mustWork = TRUE)
  expected_root <- normalizePath(
    file.path(normalized_output, "artifacts"),
    mustWork = FALSE
  )
  expected_path <- normalizePath(
    file.path(expected_root, "root"),
    mustWork = FALSE
  )
  provenance <- idata$provenance

  expect_equal(provenance$artifacts_root, expected_root)
  expect_equal(provenance$artifacts_path, expected_path)
  expect_false(dir.exists(expected_path))

  simulated_run <- file.path(provenance$artifacts_path, "distance", "run-001")
  expect_true(dir.create(simulated_run, recursive = TRUE))
  expect_true(dir.exists(simulated_run))

  metadata_json <- jsonlite::read_json(
    file.path(output_dir, "metadata.json"),
    simplifyVector = FALSE
  )
  expect_equal(metadata_json$provenance$artifacts_root, expected_root)
  expect_equal(metadata_json$provenance$artifacts_path, expected_path)

  reloaded <- read_immundata(output_dir, verbose = FALSE)
  expect_equal(reloaded$provenance$artifacts_root, expected_root)
  expect_equal(reloaded$provenance$artifacts_path, expected_path)
  expect_true(dir.exists(file.path(
    reloaded$provenance$artifacts_path,
    "distance",
    "run-001"
  )))
})

test_that("managed snapshots mirror tag and version beneath artifacts root", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  root_idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )
  snapshot <- write_immundata(
    root_idata,
    output_folder = NULL,
    tag = "baseline"
  )

  normalized_output <- normalizePath(output_dir, mustWork = TRUE)
  expected_root <- normalizePath(
    file.path(normalized_output, "artifacts"),
    mustWork = FALSE
  )
  expected_path <- normalizePath(
    file.path(expected_root, "baseline", "v001"),
    mustWork = FALSE
  )

  expect_equal(snapshot$provenance$artifacts_root, expected_root)
  expect_equal(snapshot$provenance$artifacts_path, expected_path)
  expect_false(dir.exists(expected_path))

  simulated_run <- file.path(
    snapshot$provenance$artifacts_path,
    "distance",
    "run-001"
  )
  expect_true(dir.create(simulated_run, recursive = TRUE))
  expect_true(dir.exists(simulated_run))

  snapshot_path <- file.path(
    output_dir,
    "snapshots",
    "baseline",
    "v001"
  )
  metadata_json <- jsonlite::read_json(
    file.path(snapshot_path, "metadata.json"),
    simplifyVector = FALSE
  )
  expect_equal(metadata_json$provenance$artifacts_root, expected_root)
  expect_equal(metadata_json$provenance$artifacts_path, expected_path)

  reloaded <- read_immundata(
    output_dir,
    tag = "baseline",
    version = 1,
    verbose = FALSE
  )
  expect_equal(reloaded$provenance$artifacts_root, expected_root)
  expect_equal(reloaded$provenance$artifacts_path, expected_path)
  expect_true(dir.exists(file.path(
    reloaded$provenance$artifacts_path,
    "distance",
    "run-001"
  )))
})

test_that("read_repertoires() writes metadata with lineage array and provenance", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")

  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  metadata_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)

  expect_equal(metadata_json$format_version, 2)
  expect_true(is.character(metadata_json$snapshot_id))
  expect_equal(metadata_json$producer[["function"]], "read_repertoires")
  expect_true(is.list(metadata_json$lineage))
  expect_length(metadata_json$lineage, 1)
  expect_true(is.list(metadata_json$provenance))
  expect_false("snapshot_id" %in% names(metadata_json$provenance))
  expect_false("lineage" %in% names(metadata_json$provenance))

  ingestion_event <- metadata_json$lineage[[1]]
  expect_equal(ingestion_event$event, "ingestion")
  expect_equal(ingestion_event$producer[["function"]], "read_repertoires")
  expect_equal(ingestion_event$inputs$files, normalizePath(sample_file))
  expect_false(isTRUE(ingestion_event$inputs$manifest_joined))

  normalized_out <- normalizePath(output_dir, mustWork = FALSE)
  expect_equal(metadata_json$provenance$home_path, normalized_out)
  expect_equal(metadata_json$provenance$current_path, normalized_out)
  expect_equal(
    normalizePath(metadata_json$provenance$snapshot_root, mustWork = FALSE),
    normalizePath(file.path(normalized_out, "snapshots"), mustWork = FALSE)
  )

  loaded <- read_immundata(output_dir, verbose = FALSE)
  loaded_provenance <- get_provenance(loaded)
  expect_equal(loaded_provenance$snapshot_id, metadata_json$snapshot_id)
  expect_length(loaded_provenance$lineage, length(metadata_json$lineage))
  expect_equal(
    vapply(loaded_provenance$lineage, `[[`, character(1), "event"),
    vapply(metadata_json$lineage, `[[`, character(1), "event")
  )
  expect_equal(
    vapply(loaded_provenance$lineage, `[[`, character(1), "snapshot_id"),
    vapply(metadata_json$lineage, `[[`, character(1), "snapshot_id")
  )

  metadata_json$provenance$snapshot_id <- "stale-duplicated-id"
  metadata_json$provenance$lineage <- list(list(
    event = "stale-duplicated-event",
    snapshot_id = "stale-duplicated-id"
  ))
  jsonlite::write_json(
    metadata_json,
    metadata_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  loaded_duplicated_v2 <- read_immundata(output_dir, verbose = FALSE)
  duplicated_v2_provenance <- get_provenance(loaded_duplicated_v2)
  expect_equal(duplicated_v2_provenance$snapshot_id, metadata_json$snapshot_id)
  expect_equal(
    vapply(duplicated_v2_provenance$lineage, `[[`, character(1), "event"),
    vapply(metadata_json$lineage, `[[`, character(1), "event")
  )
})

test_that("write_immundata() appends snapshot lineage event", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  before_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  expect_length(before_json$lineage, 1)
  previous_snapshot_id <- before_json$snapshot_id

  write_immundata(idata, output_folder = output_dir)
  after_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)

  expect_equal(after_json$producer[["function"]], "write_immundata")
  expect_length(after_json$lineage, 2)
  expect_false(identical(after_json$snapshot_id, previous_snapshot_id))

  snapshot_event <- after_json$lineage[[2]]
  expect_equal(snapshot_event$event, "snapshot")
  expect_equal(snapshot_event$producer[["function"]], "write_immundata")

  normalized_out <- normalizePath(output_dir, mustWork = FALSE)
  expect_equal(snapshot_event$source_path, normalized_out)
  expect_equal(snapshot_event$snapshot_path, normalized_out)
})

test_that("read_repertoires() writes manifest-derived files in ingestion lineage", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest_df <- read_manifest(manifest_path)

  read_repertoires(
    path = "<manifest>",
    schema = c("cdr3_aa", "v_call"),
    manifest = manifest_df,
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  metadata_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)

  expect_equal(metadata_json$producer[["function"]], "read_repertoires")
  expect_length(metadata_json$lineage, 1)

  ingestion_event <- metadata_json$lineage[[1]]
  expect_true(isTRUE(ingestion_event$inputs$manifest_joined))
  expect_equal(
    unlist(ingestion_event$inputs$files, use.names = FALSE),
    normalizePath(manifest_df$file)
  )
  expect_equal(ingestion_event$args$manifest_file_col, "file")
})

test_that("write_immundata() auto-creates snapshot folders and increments versions", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  idata_v1 <- write_immundata(idata, output_folder = NULL, tag = "baseline")
  idata_v2 <- write_immundata(idata_v1, output_folder = NULL, tag = "baseline")

  expect_true(dir.exists(file.path(output_dir, "snapshots", "baseline", "v001")))
  expect_true(dir.exists(file.path(output_dir, "snapshots", "baseline", "v002")))

  prov_v2 <- get_provenance(idata_v2)
  expect_equal(
    prov_v2$current_path,
    normalizePath(file.path(output_dir, "snapshots", "baseline", "v002"), mustWork = FALSE)
  )
})

test_that("snapshot tests use projectA/projectB tree in temporary snapshot root", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA
  rehome_dir <- layout$projectB

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  write_immundata(idata, output_folder = NULL, tag = "baseline")
  write_immundata(
    read_immundata(output_dir, tag = "baseline", version = 1),
    output_folder = NULL,
    tag = "baseline"
  )
  write_immundata(idata, output_folder = NULL, tag = "treated")
  write_immundata(idata, output_folder = rehome_dir, rehome = TRUE)

  expect_true(file.exists(file.path(output_dir, "annotations.parquet")))
  expect_true(file.exists(file.path(output_dir, "metadata.json")))
  expect_true(dir.exists(file.path(output_dir, "snapshots", "baseline", "v001")))
  expect_true(dir.exists(file.path(output_dir, "snapshots", "baseline", "v002")))
  expect_true(dir.exists(file.path(output_dir, "snapshots", "treated", "v001")))
  expect_true(file.exists(file.path(rehome_dir, "annotations.parquet")))
  expect_true(file.exists(file.path(rehome_dir, "metadata.json")))
})

test_that("read_immundata() resolves tag latest and specific versions", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )
  write_immundata(idata, output_folder = NULL, tag = "baseline")
  write_immundata(read_immundata(output_dir, tag = "baseline", version = 1), output_folder = NULL, tag = "baseline")

  latest <- read_immundata(output_dir, tag = "baseline")
  latest_prov <- get_provenance(latest)
  expect_equal(
    latest_prov$current_path,
    normalizePath(file.path(output_dir, "snapshots", "baseline", "v002"), mustWork = FALSE)
  )

  version1 <- read_immundata(output_dir, tag = "baseline", version = 1)
  v1_prov <- get_provenance(version1)
  expect_equal(
    v1_prov$current_path,
    normalizePath(file.path(output_dir, "snapshots", "baseline", "v001"), mustWork = FALSE)
  )
})

test_that("snapshot path resolution validates missing tags and versions", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )
  write_immundata(idata, output_folder = NULL, tag = "baseline")

  expect_error(
    read_immundata(output_dir, tag = "ghost"),
    "not found"
  )

  expect_error(
    read_immundata(output_dir, tag = "baseline", version = 99),
    "not found"
  )

  expect_error(
    read_immundata(output_dir, version = 1),
    "only.*tag"
  )

  expect_error(
    read_immundata(file.path(output_dir, "snapshots", "baseline", "v001"), tag = "baseline"),
    "already points"
  )

  expect_error(
    read_immundata(output_dir, tag = "../bad"),
    "must not include path separators"
  )

  expect_error(
    read_immundata(output_dir, tag = "bad tag"),
    "may only contain"
  )

  expect_error(
    read_immundata(output_dir, tag = "root"),
    "reserved for the original ingestion state"
  )

  expect_error(
    read_immundata(output_dir, tag = "ROOT"),
    "reserved for the original ingestion state"
  )
})

test_that("in-memory provenance reads are stable and snapshot IDs are created by writes", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  idata <- get_test_idata_tsv_no_manifest()
  idata_no_provenance <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = idata$annotations
  )

  first_provenance <- get_provenance(idata_no_provenance)
  second_provenance <- get_provenance(idata_no_provenance)
  expect_identical(first_provenance, second_provenance)
  expect_null(first_provenance$snapshot_id)
  expect_null(idata_no_provenance$.__enclos_env__$private$.provenance)

  expect_error(
    write_immundata(idata_no_provenance, output_folder = NULL),
    "Cannot infer snapshot home path"
  )

  written <- write_immundata(idata_no_provenance, output_folder = output_dir)
  metadata_json <- jsonlite::read_json(
    file.path(output_dir, "metadata.json"),
    simplifyVector = FALSE
  )
  expect_true(is.character(metadata_json$snapshot_id))
  expect_equal(get_provenance(written)$snapshot_id, metadata_json$snapshot_id)

  expect_error(
    write_immundata(idata, output_folder = NULL, tag = "../bad"),
    "must not include path separators"
  )
})

test_that("write_immundata() rehome controls future auto-snapshot root", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA
  alt_output_dir <- layout$projectB

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  moved_without_rehome <- write_immundata(idata, output_folder = alt_output_dir, rehome = FALSE)
  auto_from_old_home <- write_immundata(moved_without_rehome, output_folder = NULL, tag = "baseline")
  prov_old_home <- get_provenance(auto_from_old_home)
  expect_equal(
    prov_old_home$current_path,
    normalizePath(file.path(output_dir, "snapshots", "baseline", "v001"), mustWork = FALSE)
  )

  moved_with_rehome <- write_immundata(idata, output_folder = alt_output_dir, rehome = TRUE)
  auto_from_new_home <- write_immundata(moved_with_rehome, output_folder = NULL, tag = "baseline")
  prov_new_home <- get_provenance(auto_from_new_home)
  expect_equal(
    prov_new_home$current_path,
    normalizePath(file.path(alt_output_dir, "snapshots", "baseline", "v001"), mustWork = FALSE)
  )
})

test_that("operation outputs preserve provenance for auto-snapshots", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  idata <- read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  filtered <- filter_immundata(idata, TRUE)
  snap <- write_immundata(filtered, output_folder = NULL, tag = "ops")
  prov <- get_provenance(snap)

  expect_equal(
    prov$current_path,
    normalizePath(file.path(output_dir, "snapshots", "ops", "v001"), mustWork = FALSE)
  )

  aggregated <- agg_repertoires(idata, "imd_filename")
  downsampled <- downsample_immundata(aggregated, n = 0.5, seed = 1)
  downsampled_snap <- write_immundata(downsampled, output_folder = NULL, tag = "downsample")
  downsampled_prov <- get_provenance(downsampled_snap)

  expect_equal(
    downsampled_prov$current_path,
    normalizePath(file.path(output_dir, "snapshots", "downsample", "v001"), mustWork = FALSE)
  )
})

test_that("write/read roundtrip preserves repertoire and strata state from metadata.json", {
  output_dir <- create_test_output_dir("strata_roundtrip_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  idata <- get_test_idata() |>
    agg_repertoires(c("Response", "Therapy")) |>
    agg_strata(schema = "Response")

  strata_ids <- sort(unique(idata$repertoires[[imd_schema("strata")]]))
  custom_names <- paste0("Custom_", strata_ids)
  names(custom_names) <- as.character(strata_ids)
  idata <- rename_strata(idata, names = custom_names)

  repertoire_col <- imd_schema("repertoire")
  strata_col <- imd_schema("strata")
  shifted_annotations <- idata$annotations |>
    dplyr::mutate(
      !!rlang::sym(repertoire_col) := !!rlang::sym(repertoire_col) + 1000L,
      !!rlang::sym(strata_col) := !!rlang::sym(strata_col) + 100L
    )
  shifted_repertoires <- idata$repertoires
  shifted_repertoires[[repertoire_col]] <- shifted_repertoires[[repertoire_col]] + 1000L
  shifted_repertoires[[strata_col]] <- shifted_repertoires[[strata_col]] + 100L
  shifted_repertoires$json_flag <- rep(c(TRUE, NA), length.out = nrow(shifted_repertoires))
  shifted_repertoires$json_score <- rep(c(1.5, NA_real_), length.out = nrow(shifted_repertoires))
  shifted_repertoires$json_label <- rep(c("A", NA_character_), length.out = nrow(shifted_repertoires))
  shifted_annotations <- shifted_annotations |>
    dplyr::left_join(
      duckplyr::as_duckdb_tibble(
        shifted_repertoires |>
          dplyr::select(all_of(c(
            repertoire_col,
            "json_flag",
            "json_score",
            "json_label"
          )))
      ),
      by = repertoire_col
    )
  shifted_strata <- shifted_repertoires |>
    dplyr::select(all_of(c(
      strata_col,
      imd_schema("strata_name"),
      idata$schema_strata
    ))) |>
    dplyr::distinct() |>
    duckplyr::as_duckdb_tibble()

  original <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = shifted_annotations,
    repertoires = duckplyr::as_duckdb_tibble(shifted_repertoires),
    strata = shifted_strata,
    provenance = get_provenance(idata)
  )

  write_immundata(original, output_folder = output_dir)
  loaded <- read_immundata(output_dir, verbose = FALSE)

  metadata_json <- jsonlite::read_json(
    file.path(output_dir, "metadata.json"),
    simplifyVector = FALSE
  )
  expect_equal(
    unlist(metadata_json$schema_strata, use.names = FALSE),
    original$schema_strata
  )
  expect_true(is.list(metadata_json$repertoires))
  expect_equal(names(metadata_json$repertoires), names(original$repertoires))

  expect_equal(loaded$schema_repertoire, original$schema_repertoire)
  expect_equal(loaded$schema_strata, original$schema_strata)
  expect_equal(
    as.data.frame(loaded$repertoires),
    as.data.frame(original$repertoires),
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(loaded$strata),
    as.data.frame(original$strata),
    ignore_attr = TRUE
  )

  loaded_ids <- loaded$annotations |>
    dplyr::select(all_of(c(repertoire_col, strata_col))) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::arrange(.data[[repertoire_col]])
  original_ids <- original$annotations |>
    dplyr::select(all_of(c(repertoire_col, strata_col))) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::arrange(.data[[repertoire_col]])
  expect_equal(loaded_ids, original_ids, ignore_attr = TRUE)
})

test_that("read_immundata errors when repertoire schema lacks serialized repertoires", {
  output_dir <- create_test_output_dir("missing_repertoires_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    repertoire_schema = NULL,
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  metadata_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  metadata_json$schema_repertoire <- "imd_filename"
  metadata_json["repertoires"] <- list(NULL)
  jsonlite::write_json(
    metadata_json,
    metadata_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  expect_error(
    read_immundata(output_dir, verbose = FALSE),
    "declares a repertoire schema.*does not contain serialized repertoire data"
  )
})

test_that("read_immundata reports missing Parquet columns without collecting annotations", {
  output_dir <- create_test_output_dir("snapshot_schema_source_")
  broken_dir <- create_test_output_dir("snapshot_schema_broken_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)
  on.exit(cleanup_output_dir(broken_dir), add = TRUE)

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    repertoire_schema = NULL,
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  dir.create(broken_dir, recursive = TRUE)
  file.copy(file.path(output_dir, "metadata.json"), broken_dir)
  duckplyr::read_parquet_duckdb(file.path(output_dir, "annotations.parquet")) |>
    dplyr::select(-imd_receptor_id, -cdr3_aa) |>
    duckplyr::compute_parquet(file.path(broken_dir, "annotations.parquet"))

  error <- tryCatch(
    read_immundata(broken_dir, verbose = FALSE),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Cannot load ImmunData snapshot", fixed = TRUE)
  expect_match(conditionMessage(error), "imd_receptor_id", fixed = TRUE)
  expect_match(conditionMessage(error), "cdr3_aa", fixed = TRUE)
  expect_match(conditionMessage(error), "No data was loaded", fixed = TRUE)
})

test_that("read_immundata validates declared repertoire columns", {
  output_dir <- create_test_output_dir("snapshot_repertoire_columns_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    repertoire_schema = "imd_filename",
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  metadata_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  metadata_json$schema_repertoire <- c(metadata_json$schema_repertoire, "repertoire_only")
  metadata_json$repertoires$repertoire_only <- metadata_json$repertoires$imd_filename
  metadata_json$repertoires$n_receptors <- NULL
  jsonlite::write_json(
    metadata_json,
    metadata_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  error <- tryCatch(
    read_immundata(output_dir, verbose = FALSE),
    error = identity
  )
  expect_match(conditionMessage(error), "repertoire_only", fixed = TRUE)
  expect_match(conditionMessage(error), "n_receptors", fixed = TRUE)
})

test_that("read_immundata validates malformed and incomplete metadata fields", {
  output_dir <- create_test_output_dir("snapshot_metadata_boundary_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  valid_metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)

  malformed_metadata <- valid_metadata
  malformed_metadata$producer <- "not a metadata object"
  jsonlite::write_json(
    malformed_metadata,
    metadata_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  expect_error(read_immundata(output_dir, verbose = FALSE))

  incomplete_metadata <- valid_metadata
  incomplete_metadata$extensions <- NULL
  jsonlite::write_json(
    incomplete_metadata,
    metadata_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  expect_error(
    read_immundata(output_dir, verbose = FALSE),
    "missing required field"
  )
})

test_that("read_immundata rejects unsupported snapshot format versions", {
  output_dir <- create_test_output_dir("snapshot_unsupported_version_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  metadata_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  metadata_json$format_version <- 99L
  jsonlite::write_json(
    metadata_json,
    metadata_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  expect_error(
    read_immundata(output_dir, verbose = FALSE),
    "Unsupported.*format version"
  )
})

test_that("read_immundata rejects unreadable annotation snapshots", {
  output_dir <- create_test_output_dir("snapshot_corrupt_annotations_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  sample_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
  read_repertoires(
    path = sample_file,
    schema = c("cdr3_aa", "v_call"),
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  writeLines("not a parquet file", file.path(output_dir, "annotations.parquet"))
  expect_error(read_immundata(output_dir, verbose = FALSE))
})

test_that("read_immundata validates declared strata columns", {
  output_dir <- create_test_output_dir("snapshot_strata_columns_")
  on.exit(cleanup_output_dir(output_dir), add = TRUE)

  idata <- get_test_idata() |>
    agg_repertoires(c("Response", "Therapy")) |>
    agg_strata(schema = "Response")
  write_immundata(idata, output_folder = output_dir)

  metadata_path <- file.path(output_dir, "metadata.json")
  metadata_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  metadata_json$repertoires[[imd_schema("strata")]] <- NULL
  jsonlite::write_json(
    metadata_json,
    metadata_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  expect_error(
    read_immundata(output_dir, verbose = FALSE),
    imd_schema("strata")
  )
})
