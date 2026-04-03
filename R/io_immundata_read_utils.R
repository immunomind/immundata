is_snapshot_version_path <- function(path) {
  base <- basename(path)
  if (!grepl("^v[0-9]+$", base)) {
    return(FALSE)
  }

  snapshots_dir <- basename(dirname(dirname(path)))
  identical(snapshots_dir, "snapshots")
}

list_available_snapshot_tags <- function(home_path) {
  snapshots_root <- file.path(home_path, "snapshots")
  if (!dir.exists(snapshots_root)) {
    return(character())
  }

  tags <- list.files(snapshots_root, full.names = FALSE, recursive = FALSE, all.files = FALSE)
  tags <- tags[file.info(file.path(snapshots_root, tags))$isdir %in% TRUE]
  sort(tags)
}

resolve_snapshot_input_path <- function(path, tag = NULL, version = NULL) {
  checkmate::assert_character(path, len = 1, null.ok = FALSE)
  checkmate::assert_character(tag, len = 1, null.ok = TRUE)
  checkmate::assert_count(version, null.ok = TRUE)

  path <- normalizePath(path, mustWork = FALSE)

  if (!is.null(version) && is.null(tag)) {
    cli::cli_abort("`version` can only be used together with {.arg tag}.")
  }

  if (is.null(tag)) {
    return(path)
  }

  if (is_snapshot_version_path(path)) {
    cli::cli_abort(
      "Path [{path}] already points to a concrete snapshot version folder; do not combine it with {.arg tag}/{.arg version}."
    )
  }

  tag <- imd_validate_snapshot_tag(tag)
  tag_dir <- file.path(path, "snapshots", tag)

  if (!dir.exists(tag_dir)) {
    available_tags <- list_available_snapshot_tags(path)
    if (length(available_tags) == 0) {
      cli::cli_abort(
        "Snapshot tag [{tag}] was not found under [{path}/snapshots]. No snapshot tags are available."
      )
    }

    cli::cli_abort(
      "Snapshot tag [{tag}] was not found under [{path}/snapshots]. Available tags: [{available_tags}]."
    )
  }

  available_versions <- imd_list_snapshot_versions(tag_dir)
  if (length(available_versions) == 0) {
    cli::cli_abort(
      "Snapshot tag [{tag}] exists under [{tag_dir}] but has no version directories (expected vNNN)."
    )
  }

  if (is.null(version)) {
    version <- max(available_versions)
  }

  if (!version %in% available_versions) {
    formatted <- imd_format_snapshot_version(available_versions)
    cli::cli_abort(
      "Snapshot version [{imd_format_snapshot_version(version)}] was not found for tag [{tag}]. Available versions: [{formatted}]."
    )
  }

  file.path(tag_dir, imd_format_snapshot_version(version))
}

normalize_json_character_field <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x)) {
    return(unname(x))
  }
  if (is.list(x)) {
    vals <- unname(unlist(x, recursive = TRUE, use.names = FALSE))
    if (length(vals) == 0) {
      return(NULL)
    }
    return(as.character(vals))
  }
  cli::cli_abort("Expected a character field in metadata JSON, got type [{typeof(x)}].")
}

is_legacy_metadata_v1 <- function(meta_raw) {
  has_legacy_schema <- all(c("receptor_schema", "repertoire_schema") %in% names(meta_raw))
  has_v2_schema <- any(c("format_version", "schema_receptor", "snapshot_id", "lineage", "provenance", "extensions") %in% names(meta_raw))
  has_legacy_schema && !has_v2_schema
}

upgrade_metadata_v1_to_v2 <- function(meta_raw) {
  checkmate::assert_true(is_legacy_metadata_v1(meta_raw))

  cli::cli_warn("Detected legacy v1 metadata.json. Upgrading to v2 in memory for loading.")

  package_version <- normalize_json_character_field(meta_raw$version)
  if (is.null(package_version) || length(package_version) == 0) {
    package_version <- as.character(packageVersion("immundata"))
  } else {
    package_version <- package_version[[1]]
  }

  receptor_schema <- meta_raw$receptor_schema
  checkmate::assert_list(receptor_schema)
  checkmate::assert_names(names(receptor_schema), must.include = "features")
  receptor_schema$features <- normalize_json_character_field(receptor_schema$features)
  checkmate::assert_character(receptor_schema$features, min.len = 1)
  receptor_schema$chains <- normalize_json_character_field(receptor_schema$chains)

  list(
    format_version = 2L,
    package_version = package_version,
    schema_receptor = receptor_schema,
    schema_repertoire = normalize_json_character_field(meta_raw$repertoire_schema),
    producer = list("function" = "metadata_upgrade_v1"),
    snapshot_id = imd_generate_snapshot_id(),
    lineage = list(),
    provenance = list(),
    extensions = list(
      legacy = list(
        source_format_version = 1L,
        source_package_version = package_version
      )
    )
  )
}

normalize_metadata_json <- function(meta_raw) {
  checkmate::assert_list(meta_raw)

  if (is_legacy_metadata_v1(meta_raw)) {
    meta_raw <- upgrade_metadata_v1_to_v2(meta_raw)
  }

  required_fields <- c(
    "format_version", "package_version", "schema_receptor", "schema_repertoire",
    "producer", "snapshot_id", "lineage", "provenance", "extensions"
  )
  missing <- setdiff(required_fields, names(meta_raw))
  if (length(missing) > 0) {
    cli::cli_abort(
      "metadata.json is missing required field(s): [{missing}]."
    )
  }

  checkmate::assert_count(meta_raw$format_version)
  checkmate::assert_character(meta_raw$package_version, len = 1)
  checkmate::assert_list(meta_raw$schema_receptor)
  checkmate::assert(
    checkmate::test_list(meta_raw$schema_repertoire, null.ok = TRUE),
    checkmate::test_character(meta_raw$schema_repertoire, null.ok = TRUE)
  )
  checkmate::assert_list(meta_raw$producer)
  checkmate::assert_character(meta_raw$snapshot_id, len = 1)
  checkmate::assert_list(meta_raw$lineage)
  checkmate::assert_list(meta_raw$provenance)
  checkmate::assert_list(meta_raw$extensions)

  for (event in meta_raw$lineage) {
    checkmate::assert_list(event)
  }

  schema_receptor <- meta_raw$schema_receptor
  checkmate::assert_names(names(schema_receptor), must.include = "features")
  schema_receptor$features <- normalize_json_character_field(schema_receptor$features)
  checkmate::assert_character(schema_receptor$features, min.len = 1)
  schema_receptor$chains <- normalize_json_character_field(schema_receptor$chains)
  meta_raw$schema_receptor <- schema_receptor

  meta_raw$schema_repertoire <- normalize_json_character_field(meta_raw$schema_repertoire)

  meta_raw$provenance <- normalize_provenance(
    meta_raw$provenance,
    fallback_snapshot_id = meta_raw$snapshot_id,
    fallback_lineage = meta_raw$lineage
  )
  meta_raw
}
