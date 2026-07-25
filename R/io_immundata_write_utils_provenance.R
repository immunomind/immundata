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
