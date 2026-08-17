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
    schema_strata = NULL,
    producer = list("function" = "metadata_upgrade_v1"),
    snapshot_id = NULL,
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

normalize_metadata_v2 <- function(meta_raw) {
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

  checkmate::assert_number(meta_raw$format_version, lower = 2, upper = 2)
  checkmate::assert_character(meta_raw$package_version, len = 1)
  checkmate::assert_list(meta_raw$schema_receptor)
  checkmate::assert(
    checkmate::test_list(meta_raw$schema_repertoire, null.ok = TRUE),
    checkmate::test_character(meta_raw$schema_repertoire, null.ok = TRUE)
  )
  checkmate::assert_list(meta_raw$producer)
  is_legacy_upgrade <- !is.null(meta_raw$extensions$legacy)
  if (is.null(meta_raw$snapshot_id) && !is_legacy_upgrade) {
    cli::cli_abort("metadata.json field [snapshot_id] must be a character scalar.")
  }
  checkmate::assert_character(meta_raw$snapshot_id, len = 1, null.ok = is_legacy_upgrade)
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
  has_serialized_repertoires <- "repertoires" %in% names(meta_raw)
  meta_raw$schema_strata <- normalize_json_character_field(meta_raw$schema_strata)
  if (!"schema_strata" %in% names(meta_raw)) {
    meta_raw$schema_strata <- NULL
  }
  if (!is.null(meta_raw$schema_strata)) {
    checkmate::assert_character(meta_raw$schema_strata)
  }

  if (has_serialized_repertoires) {
    checkmate::assert_list(meta_raw$repertoires, null.ok = TRUE)
  } else {
    meta_raw$repertoires <- NULL
  }
  if (!is.null(meta_raw$repertoires)) {
    meta_raw$repertoires <- as.data.frame(
      meta_raw$repertoires,
      stringsAsFactors = FALSE,
      optional = TRUE,
      check.names = FALSE
    )
  }

  if (!is.null(meta_raw$schema_repertoire) && has_serialized_repertoires && is.null(meta_raw$repertoires)) {
    cli::cli_abort(
      "Snapshot declares a repertoire schema but does not contain serialized repertoire data."
    )
  }
  if (is.null(meta_raw$schema_repertoire) && !is.null(meta_raw$repertoires)) {
    cli::cli_abort(
      "Snapshot contains serialized repertoire data but does not declare a repertoire schema."
    )
  }
  if (!is.null(meta_raw$schema_strata) && is.null(meta_raw$schema_repertoire)) {
    cli::cli_abort(
      "Snapshot declares a strata schema but does not declare a repertoire schema."
    )
  }

  # Old v2 snapshots can contain duplicated snapshot state in provenance.
  # Preserve path information only; top-level metadata remains canonical.
  meta_raw$provenance <- provenance_paths_for_metadata(meta_raw$provenance)
  meta_raw$rebuild_repertoires <- !has_serialized_repertoires &&
    !is.null(meta_raw$schema_repertoire)
  meta_raw
}

normalize_metadata_json <- function(meta_raw) {
  checkmate::assert_list(meta_raw)

  if (is_legacy_metadata_v1(meta_raw)) {
    return(normalize_metadata_v2(upgrade_metadata_v1_to_v2(meta_raw)))
  }

  if (!"format_version" %in% names(meta_raw)) {
    cli::cli_abort("metadata.json is missing required field(s): [format_version].")
  }
  checkmate::assert_count(meta_raw$format_version)
  if (!identical(as.integer(meta_raw$format_version), 2L)) {
    cli::cli_abort(
      "Unsupported ImmunData snapshot format version [{meta_raw$format_version}]. Supported versions: [1, 2]."
    )
  }

  normalize_metadata_v2(meta_raw)
}

validate_snapshot_columns <- function(metadata_json, annotation_data, snapshot_path) {
  annotation_columns <- colnames(annotation_data)
  issues <- character()

  receptor_schema <- metadata_json$schema_receptor
  required_annotation_columns <- c(
    imd_schema("receptor"),
    imd_schema("barcode"),
    imd_schema("chain"),
    imd_schema("chain_count"),
    imd_receptor_features(receptor_schema)
  )
  if (length(imd_receptor_chains(receptor_schema)) == 2) {
    required_annotation_columns <- c(required_annotation_columns, imd_schema("locus"))
  }

  issues <- c(
    issues,
    format_missing_columns_issue(
      setdiff(unique(required_annotation_columns), annotation_columns),
      "annotations.parquet"
    )
  )

  repertoire_schema <- metadata_json$schema_repertoire
  repertoire_data <- metadata_json$repertoires
  if (!is.null(repertoire_schema)) {
    issues <- c(
      issues,
      format_missing_columns_issue(
        setdiff(
          c(
            repertoire_schema,
            imd_schema("repertoire"),
            imd_schema("count"),
            imd_schema("proportion"),
            imd_schema("n_repertoires")
          ),
          annotation_columns
        ),
        "annotations.parquet for the declared repertoire schema"
      )
    )
    if (!isTRUE(metadata_json$rebuild_repertoires)) {
      issues <- c(
        issues,
        format_missing_columns_issue(
          setdiff(
            c(
              repertoire_schema,
              imd_schema("repertoire"),
              imd_schema("n_barcodes"),
              imd_schema("n_receptors")
            ),
            colnames(repertoire_data)
          ),
          "metadata.json repertoires"
        )
      )
    }
  }

  strata_schema <- metadata_json$schema_strata
  if (!is.null(strata_schema)) {
    issues <- c(
      issues,
      format_missing_columns_issue(
        setdiff(imd_schema("strata"), annotation_columns),
        "annotations.parquet for the declared strata schema"
      ),
      format_missing_columns_issue(
        setdiff(
          c(
            strata_schema,
            imd_schema("strata"),
            imd_schema("strata_name")
          ),
          colnames(repertoire_data)
        ),
        "metadata.json repertoires for the declared strata schema"
      )
    )
  }

  if (length(issues) > 0) {
    cli::cli_abort(c(
      "Cannot load ImmunData snapshot because its schema is inconsistent.",
      stats::setNames(issues, rep("x", length(issues))),
      "i" = "Snapshot: {.path {snapshot_path}}",
      "i" = "No data was loaded. Recreate the snapshot or correct its declared schema."
    ))
  }

  invisible(TRUE)
}

format_missing_columns_issue <- function(missing, location) {
  if (length(missing) == 0) {
    return(character())
  }

  paste0(
    location,
    " is missing required column(s): ",
    paste(missing, collapse = ", "),
    "."
  )
}
