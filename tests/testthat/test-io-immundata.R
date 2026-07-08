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
    repertoire_schema = list()
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

  prov <- imd_get_provenance(idata)
  expect_equal(prov$current_path, normalizePath(legacy_path, mustWork = FALSE))
  expect_true(is.character(prov$snapshot_id))
  expect_gt(nchar(prov$snapshot_id), 0)
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

  expect_identical(idata$provenance, imd_get_provenance(idata))

  expect_error(
    idata$provenance <- list(),
    "read-only"
  )
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

  ingestion_event <- metadata_json$lineage[[1]]
  expect_equal(ingestion_event$event, "ingestion")
  expect_equal(ingestion_event$producer[["function"]], "read_repertoires")
  expect_equal(ingestion_event$inputs$files, normalizePath(sample_file))
  expect_false(isTRUE(ingestion_event$inputs$metadata_joined))

  normalized_out <- normalizePath(output_dir, mustWork = FALSE)
  expect_equal(metadata_json$provenance$home_path, normalized_out)
  expect_equal(metadata_json$provenance$current_path, normalized_out)
  expect_equal(
    normalizePath(metadata_json$provenance$snapshot_root, mustWork = FALSE),
    normalizePath(file.path(normalized_out, "snapshots"), mustWork = FALSE)
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

test_that("read_repertoires() writes metadata-derived files in ingestion lineage", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
  metadata_df <- read_metadata(md_path)

  read_repertoires(
    path = "<metadata>",
    schema = c("cdr3_aa", "v_call"),
    metadata = metadata_df,
    output_folder = output_dir,
    preprocess = NULL,
    postprocess = NULL
  )

  metadata_path <- file.path(output_dir, "metadata.json")
  metadata_json <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)

  expect_equal(metadata_json$producer[["function"]], "read_repertoires")
  expect_length(metadata_json$lineage, 1)

  ingestion_event <- metadata_json$lineage[[1]]
  expect_true(isTRUE(ingestion_event$inputs$metadata_joined))
  expect_equal(
    unlist(ingestion_event$inputs$files, use.names = FALSE),
    normalizePath(metadata_df$File)
  )
  expect_equal(ingestion_event$args$metadata_file_col, "File")
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

  prov_v2 <- imd_get_provenance(idata_v2)
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
  latest_prov <- imd_get_provenance(latest)
  expect_equal(
    latest_prov$current_path,
    normalizePath(file.path(output_dir, "snapshots", "baseline", "v002"), mustWork = FALSE)
  )

  version1 <- read_immundata(output_dir, tag = "baseline", version = 1)
  v1_prov <- imd_get_provenance(version1)
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
})

test_that("write_immundata() validates tags and missing provenance for auto-snapshots", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  idata <- get_test_idata_tsv_no_metadata()
  annotations_tbl <- idata$annotations |> collect()
  idata_no_provenance <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = annotations_tbl
  )

  expect_error(
    write_immundata(idata_no_provenance, output_folder = NULL),
    "Cannot infer snapshot home path"
  )

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
  prov_old_home <- imd_get_provenance(auto_from_old_home)
  expect_equal(
    prov_old_home$current_path,
    normalizePath(file.path(output_dir, "snapshots", "baseline", "v001"), mustWork = FALSE)
  )

  moved_with_rehome <- write_immundata(idata, output_folder = alt_output_dir, rehome = TRUE)
  auto_from_new_home <- write_immundata(moved_with_rehome, output_folder = NULL, tag = "baseline")
  prov_new_home <- imd_get_provenance(auto_from_new_home)
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
  prov <- imd_get_provenance(snap)

  expect_equal(
    prov$current_path,
    normalizePath(file.path(output_dir, "snapshots", "ops", "v001"), mustWork = FALSE)
  )

  aggregated <- agg_repertoires(idata, "filename")
  downsampled <- downsample_immundata(aggregated, n = 0.5, seed = 1)
  downsampled_snap <- write_immundata(downsampled, output_folder = NULL, tag = "downsample")
  downsampled_prov <- imd_get_provenance(downsampled_snap)

  expect_equal(
    downsampled_prov$current_path,
    normalizePath(file.path(output_dir, "snapshots", "downsample", "v001"), mustWork = FALSE)
  )
})

test_that("write_immundata_internal() validates lineage as complete set", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  idata <- get_test_idata_tsv_no_metadata()

  expect_error(
    write_immundata_internal(
      idata = idata,
      output_folder = output_dir,
      producer_function = "read_repertoires",
      metadata_lineage_inputs = list(
        files = c("/tmp/sample.tsv"),
        metadata_joined = FALSE,
        enforce_schema = TRUE
      )
    ),
    "complete set"
  )
})

test_that("write_immundata_internal() validates lineage fields", {
  layout <- create_snapshot_test_layout()
  on.exit(cleanup_snapshot_test_root())
  output_dir <- layout$projectA

  idata <- get_test_idata_tsv_no_metadata()

  expect_error(
    write_immundata_internal(
      idata = idata,
      output_folder = output_dir,
      producer_function = "read_repertoires",
      metadata_lineage_inputs = list(
        files = c("/tmp/sample.tsv"),
        metadata_joined = FALSE,
        enforce_schema = TRUE
      ),
      metadata_lineage_args = list(
        barcode_col = NULL,
        count_col = NULL,
        locus_col = NULL,
        umi_col = NULL
      ),
      metadata_lineage_columns = list(
        renamed = list(requested = character(), applied = character(), not_found = character()),
        dropped = list(applied = character())
      ),
      metadata_lineage_pipeline = list(preprocess = character(), postprocess = character())
    ),
    "metadata_file_col|must include"
  )
})
