is_legacy_metadata_json <- function(meta_raw) {
  is.null(meta_raw[["format_version"]]) &&
    (!is.null(meta_raw[["version"]]) ||
      !is.null(meta_raw[["receptor_schema"]]) ||
      !is.null(meta_raw[["repertoire_schema"]]))
}

normalize_metadata_json <- function(meta_raw) {
  metadata_json <- list(
    format_version = if (!is.null(meta_raw[["format_version"]])) meta_raw[["format_version"]] else 2L,
    package_version = if (!is.null(meta_raw[["package_version"]])) meta_raw[["package_version"]] else meta_raw[["version"]],
    schema_receptor = if (!is.null(meta_raw[["schema_receptor"]])) meta_raw[["schema_receptor"]] else meta_raw[["receptor_schema"]],
    schema_repertoire = if (!is.null(meta_raw[["schema_repertoire"]])) meta_raw[["schema_repertoire"]] else meta_raw[["repertoire_schema"]],
    producer = meta_raw[["producer"]],
    lineage = meta_raw[["lineage"]],
    extensions = if (!is.null(meta_raw[["extensions"]])) meta_raw[["extensions"]] else list()
  )

  if (is.null(metadata_json$producer)) {
    metadata_json$producer <- list("function" = "unknown")
  }

  known_fields <- c(
    "format_version", "package_version", "schema_receptor", "schema_repertoire",
    "producer", "lineage", "extensions",
    "version", "receptor_schema", "repertoire_schema"
  )
  extra_fields <- setdiff(names(meta_raw), known_fields)
  if (length(extra_fields) > 0) {
    for (field_name in extra_fields) {
      metadata_json$extensions[[field_name]] <- meta_raw[[field_name]]
    }
  }

  if (is.null(metadata_json$schema_receptor)) {
    cli::cli_abort("metadata.json is missing receptor schema.")
  }

  metadata_json
}

archive_legacy_metadata_json <- function(metadata_path) {
  backup_path <- file.path(dirname(metadata_path), "metadata_v1.json")

  if (file.exists(backup_path)) {
    file.remove(backup_path)
  }

  ok <- file.rename(metadata_path, backup_path)
  if (!ok) {
    ok <- file.copy(metadata_path, backup_path, overwrite = TRUE) && file.remove(metadata_path)
  }

  if (!ok) {
    cli::cli_abort("Failed to archive legacy metadata to {.path {backup_path}}.")
  }

  backup_path
}
