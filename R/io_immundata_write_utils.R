imd_now_utc_iso <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

imd_random_suffix <- function(n = 8L) {
  paste0(sample(c(letters, 0:9), n, replace = TRUE), collapse = "")
}

imd_generate_snapshot_id <- function() {
  paste0("imd_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_", imd_random_suffix())
}

imd_default_provenance <- function() {
  list(
    home_path = NULL,
    current_path = NULL,
    snapshot_root = NULL,
    artifacts_root = NULL,
    artifacts_path = NULL,
    snapshot_id = NULL,
    lineage = list()
  )
}

normalize_nullable_path <- function(path) {
  if (is.null(path)) {
    return(NULL)
  }

  checkmate::assert_character(path, len = 1, null.ok = FALSE)
  normalizePath(path, mustWork = FALSE)
}

imd_resolve_artifacts_path <- function(home_path,
                                       current_path,
                                       snapshot_root,
                                       snapshot_id = NULL) {
  if (is.null(home_path) || is.null(current_path) || is.null(snapshot_root)) {
    return(NULL)
  }

  home_path <- normalizePath(home_path, mustWork = FALSE)
  current_path <- normalizePath(current_path, mustWork = FALSE)
  snapshot_root <- normalizePath(snapshot_root, mustWork = FALSE)
  artifacts_root <- normalizePath(file.path(home_path, "artifacts"), mustWork = FALSE)

  if (identical(current_path, home_path)) {
    return(normalizePath(file.path(artifacts_root, "root"), mustWork = FALSE))
  }

  is_managed_snapshot <- grepl("^v[0-9]+$", basename(current_path)) &&
    identical(
      normalizePath(dirname(dirname(current_path)), mustWork = FALSE),
      snapshot_root
    )
  if (is_managed_snapshot) {
    return(normalizePath(
      file.path(artifacts_root, basename(dirname(current_path)), basename(current_path)),
      mustWork = FALSE
    ))
  }

  if (!is.null(snapshot_id)) {
    return(normalizePath(
      file.path(artifacts_root, "by-id", snapshot_id),
      mustWork = FALSE
    ))
  }

  NULL
}

normalize_provenance <- function(provenance = NULL,
                                 fallback_home_path = NULL,
                                 home_path = NULL,
                                 current_path = NULL,
                                 snapshot_id = NULL,
                                 lineage = NULL) {
  if (is.null(provenance)) {
    provenance <- list()
  }

  # Validate every supplied value before resolving precedence or deriving paths.
  checkmate::assert_list(provenance)

  path_fields <- c(
    "home_path", "current_path", "snapshot_root",
    "artifacts_root", "artifacts_path"
  )
  for (field in path_fields) {
    checkmate::assert_character(provenance[[field]], len = 1, null.ok = TRUE)
  }
  checkmate::assert_character(fallback_home_path, len = 1, null.ok = TRUE)
  checkmate::assert_character(home_path, len = 1, null.ok = TRUE)
  checkmate::assert_character(current_path, len = 1, null.ok = TRUE)
  checkmate::assert_character(provenance$snapshot_id, len = 1, null.ok = TRUE)
  checkmate::assert_character(snapshot_id, len = 1, null.ok = TRUE)
  checkmate::assert_list(provenance$lineage, null.ok = TRUE)
  checkmate::assert_list(lineage, null.ok = TRUE)
  for (event in c(provenance$lineage, lineage)) {
    checkmate::assert_list(event)
  }

  # Explicit arguments override stored provenance. The fallback is used only
  # when neither supplies a home path, as with legacy metadata.
  resolved_home_path <- if (!is.null(home_path)) {
    home_path
  } else if (!is.null(provenance$home_path)) {
    provenance$home_path
  } else {
    fallback_home_path
  }
  resolved_current_path <- if (!is.null(current_path)) {
    current_path
  } else {
    provenance$current_path
  }
  resolved_snapshot_id <- if (!is.null(snapshot_id)) {
    snapshot_id
  } else {
    provenance$snapshot_id
  }
  resolved_lineage <- if (!is.null(lineage)) {
    lineage
  } else if (!is.null(provenance$lineage)) {
    provenance$lineage
  } else {
    list()
  }

  resolved_home_path <- normalize_nullable_path(resolved_home_path)
  resolved_current_path <- normalize_nullable_path(resolved_current_path)

  # These locations are derived from the canonical home/current state and are
  # never accepted as independent sources of truth.
  snapshot_root <- if (is.null(resolved_home_path)) {
    NULL
  } else {
    normalizePath(file.path(resolved_home_path, "snapshots"), mustWork = FALSE)
  }
  artifacts_root <- if (is.null(resolved_home_path)) {
    NULL
  } else {
    normalizePath(file.path(resolved_home_path, "artifacts"), mustWork = FALSE)
  }
  artifacts_path <- imd_resolve_artifacts_path(
    home_path = resolved_home_path,
    current_path = resolved_current_path,
    snapshot_root = snapshot_root,
    snapshot_id = resolved_snapshot_id
  )

  list(
    home_path = resolved_home_path,
    current_path = resolved_current_path,
    snapshot_root = snapshot_root,
    artifacts_root = artifacts_root,
    artifacts_path = artifacts_path,
    snapshot_id = resolved_snapshot_id,
    lineage = resolved_lineage
  )
}

imd_provenance_paths_for_metadata <- function(provenance) {
  # Snapshot identity and lineage are canonical top-level metadata fields.
  # Keep only normalized location fields in metadata$provenance.
  path_fields <- c(
    "home_path", "current_path", "snapshot_root",
    "artifacts_root", "artifacts_path"
  )
  normalize_provenance(provenance)[path_fields]
}

imd_get_provenance <- function(idata) {
  checkmate::assert_r6(idata, "ImmunData")
  private_env <- idata$.__enclos_env__$private
  raw <- private_env$.provenance
  if (is.null(raw)) {
    return(imd_default_provenance())
  }

  raw
}

imd_set_provenance <- function(idata, provenance, ...) {
  checkmate::assert_r6(idata, "ImmunData")
  normalized <- normalize_provenance(provenance, ...)
  idata$.__enclos_env__$private$.provenance <- normalized
  invisible(idata)
}

imd_validate_snapshot_tag <- function(tag) {
  checkmate::assert_character(tag, len = 1, null.ok = FALSE)
  tag <- trimws(tag)
  if (identical(tag, "")) {
    cli::cli_abort("Snapshot {.arg tag} must be a non-empty string.")
  }

  if (tag %in% c(".", "..") || grepl("[/\\\\]", tag)) {
    cli::cli_abort("Snapshot {.arg tag} must not include path separators or reserved values '.'/'..'.")
  }

  if (identical(tolower(tag), "root")) {
    cli::cli_abort(
      "Snapshot {.arg tag} [root] is reserved for the original ingestion state."
    )
  }

  if (!grepl("^[A-Za-z0-9._-]+$", tag)) {
    cli::cli_abort(
      "Snapshot {.arg tag} may only contain letters, numbers, dot, underscore, and dash."
    )
  }

  tag
}

imd_format_snapshot_version <- function(version) {
  checkmate::assert_count(version)
  sprintf("v%03d", as.integer(version))
}

imd_parse_snapshot_version <- function(version_dirname) {
  if (!grepl("^v[0-9]+$", version_dirname)) {
    return(NA_integer_)
  }

  as.integer(sub("^v", "", version_dirname))
}

imd_list_snapshot_versions <- function(tag_dir) {
  if (!dir.exists(tag_dir)) {
    return(integer())
  }

  children <- list.files(tag_dir, full.names = FALSE, recursive = FALSE, all.files = FALSE)
  versions <- vapply(children, imd_parse_snapshot_version, integer(1))
  versions <- versions[!is.na(versions)]
  sort(unique(versions))
}

imd_next_snapshot_version <- function(tag_dir) {
  versions <- imd_list_snapshot_versions(tag_dir)
  if (length(versions) == 0) {
    return(1L)
  }
  max(versions) + 1L
}

imd_is_snapshot_version_path <- function(path) {
  grepl("^v[0-9]+$", basename(path)) &&
    identical(basename(dirname(dirname(path))), "snapshots")
}

imd_list_snapshot_tags <- function(home_path) {
  snapshot_root <- file.path(home_path, "snapshots")
  if (!dir.exists(snapshot_root)) {
    return(character())
  }

  tags <- list.files(snapshot_root, full.names = FALSE, recursive = FALSE, all.files = FALSE)
  tags[file.info(file.path(snapshot_root, tags))$isdir %in% TRUE] |> sort()
}

imd_resolve_snapshot_version <- function(home_path, tag, version = NULL, allocate = FALSE) {
  checkmate::assert_character(home_path, len = 1, null.ok = FALSE)
  checkmate::assert_character(tag, len = 1, null.ok = FALSE)
  checkmate::assert_count(version, null.ok = TRUE)
  checkmate::assert_flag(allocate)

  home_path <- normalizePath(home_path, mustWork = FALSE)
  tag <- imd_validate_snapshot_tag(tag)
  tag_dir <- file.path(home_path, "snapshots", tag)

  if (allocate) {
    dir.create(tag_dir, recursive = TRUE, showWarnings = FALSE)
    return(file.path(tag_dir, imd_format_snapshot_version(imd_next_snapshot_version(tag_dir))))
  }

  if (!dir.exists(tag_dir)) {
    available_tags <- imd_list_snapshot_tags(home_path)
    if (length(available_tags) == 0) {
      cli::cli_abort(
        "Snapshot tag [{tag}] was not found under [{home_path}/snapshots]. No snapshot tags are available."
      )
    }
    cli::cli_abort(
      "Snapshot tag [{tag}] was not found under [{home_path}/snapshots]. Available tags: [{available_tags}]."
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

imd_resolve_snapshot_input <- function(path, tag = NULL, version = NULL) {
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
  if (imd_is_snapshot_version_path(path)) {
    cli::cli_abort(
      "Path [{path}] already points to a concrete snapshot version folder; do not combine it with {.arg tag}/{.arg version}."
    )
  }

  imd_resolve_snapshot_version(path, tag, version, allocate = FALSE)
}

imd_resolve_snapshot_output_folder <- function(idata,
                                               output_folder = NULL,
                                               tag = NULL,
                                               rehome = FALSE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(output_folder, len = 1, null.ok = TRUE)
  checkmate::assert_character(tag, len = 1, null.ok = TRUE)
  checkmate::assert_flag(rehome)

  provenance <- imd_get_provenance(idata)

  if (!is.null(output_folder)) {
    return(list(
      output_folder = normalizePath(output_folder, mustWork = FALSE),
      tag = if (is.null(tag)) NULL else imd_validate_snapshot_tag(tag),
      provenance = provenance,
      output_was_auto = FALSE
    ))
  }

  if (rehome) {
    cli::cli_abort("`rehome = TRUE` requires an explicit {.arg output_folder}.")
  }

  if (is.null(provenance$home_path)) {
    cli::cli_abort(
      "Cannot infer snapshot home path from `idata`. Please provide {.arg output_folder} or load data using `read_immundata()` / `read_repertoires()` first."
    )
  }

  if (is.null(tag)) {
    tag <- "default"
  }
  tag <- imd_validate_snapshot_tag(tag)

  snapshot_folder <- imd_resolve_snapshot_version(
    home_path = provenance$home_path,
    tag = tag,
    allocate = TRUE
  )

  list(
    output_folder = normalizePath(snapshot_folder, mustWork = FALSE),
    tag = tag,
    provenance = provenance,
    output_was_auto = TRUE
  )
}

build_snapshot_metadata <- function(idata,
                                    producer_function,
                                    provenance_before,
                                    output_folder,
                                    snapshot_tag = NULL,
                                    rehome = FALSE,
                                    ingestion_payload = NULL,
                                    metadata_extensions = NULL) {
  snapshot_id <- imd_generate_snapshot_id()
  is_ingestion <- identical(producer_function, "read_repertoires")
  event <- list(
    event = if (is_ingestion) "ingestion" else "snapshot",
    created_at = imd_now_utc_iso(),
    snapshot_id = snapshot_id,
    producer = list("function" = producer_function)
  )
  if (is_ingestion) {
    event <- c(event, ingestion_payload)
  } else {
    event$source_path <- provenance_before$current_path
    event$snapshot_path <- output_folder
    event$tag <- snapshot_tag
  }

  lineage <- c(provenance_before$lineage, list(event))
  home_path <- provenance_before$home_path
  if (is.null(home_path) || is_ingestion || isTRUE(rehome)) {
    home_path <- output_folder
  }
  provenance_after <- normalize_provenance(
    provenance_before,
    home_path = home_path,
    current_path = output_folder,
    snapshot_id = snapshot_id,
    lineage = lineage
  )

  list(
    metadata = list(
      format_version = 2L,
      package_version = as.character(packageVersion("immundata")),
      schema_receptor = idata$schema_receptor,
      schema_repertoire = idata$schema_repertoire,
      schema_strata = idata$schema_strata,
      repertoires = serialize_repertoires_json(idata$repertoires),
      producer = list("function" = producer_function),
      snapshot_id = snapshot_id,
      lineage = lineage,
      provenance = imd_provenance_paths_for_metadata(provenance_after),
      extensions = if (is.null(metadata_extensions)) list() else metadata_extensions
    ),
    provenance = provenance_after
  )
}

serialize_repertoires_json <- function(repertoires) {
  if (is.null(repertoires)) {
    return(NULL)
  }

  checkmate::assert_data_frame(repertoires)

  columns <- as.list(repertoires)
  factor_columns <- vapply(columns, is.factor, logical(1))
  columns[factor_columns] <- lapply(columns[factor_columns], as.character)
  columns
}
