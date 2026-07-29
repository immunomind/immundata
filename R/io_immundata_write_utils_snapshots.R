validate_snapshot_tag <- function(tag) {
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

format_snapshot_version <- function(version) {
  checkmate::assert_count(version)
  sprintf("v%03d", as.integer(version))
}

list_snapshot_versions <- function(tag_dir) {
  if (!dir.exists(tag_dir)) {
    return(integer())
  }

  children <- list.files(tag_dir, full.names = FALSE, recursive = FALSE, all.files = FALSE)
  version_dirnames <- children[grepl("^v[0-9]+$", children)]
  versions <- as.integer(sub("^v", "", version_dirnames))
  sort(unique(versions))
}

list_snapshot_tags <- function(home_path) {
  snapshot_root <- file.path(home_path, "snapshots")
  if (!dir.exists(snapshot_root)) {
    return(character())
  }

  tags <- list.files(snapshot_root, full.names = FALSE, recursive = FALSE, all.files = FALSE)
  tags[file.info(file.path(snapshot_root, tags))$isdir %in% TRUE] |> sort()
}

resolve_snapshot_version <- function(home_path, tag, version = NULL, allocate = FALSE) {
  checkmate::assert_character(home_path, len = 1, null.ok = FALSE)
  checkmate::assert_character(tag, len = 1, null.ok = FALSE)
  checkmate::assert_count(version, null.ok = TRUE)
  checkmate::assert_flag(allocate)

  home_path <- normalizePath(home_path, mustWork = FALSE)
  tag <- validate_snapshot_tag(tag)
  tag_dir <- file.path(home_path, "snapshots", tag)

  if (allocate) {
    dir.create(tag_dir, recursive = TRUE, showWarnings = FALSE)
    versions <- list_snapshot_versions(tag_dir)
    next_version <- if (length(versions) == 0) 1L else max(versions) + 1L
    return(file.path(tag_dir, format_snapshot_version(next_version)))
  }

  if (!dir.exists(tag_dir)) {
    available_tags <- list_snapshot_tags(home_path)
    if (length(available_tags) == 0) {
      cli::cli_abort(
        "Snapshot tag [{tag}] was not found under [{home_path}/snapshots]. No snapshot tags are available."
      )
    }
    cli::cli_abort(
      "Snapshot tag [{tag}] was not found under [{home_path}/snapshots]. Available tags: [{available_tags}]."
    )
  }

  available_versions <- list_snapshot_versions(tag_dir)
  if (length(available_versions) == 0) {
    cli::cli_abort(
      "Snapshot tag [{tag}] exists under [{tag_dir}] but has no version directories (expected vNNN)."
    )
  }

  if (is.null(version)) {
    version <- max(available_versions)
  }
  if (!version %in% available_versions) {
    formatted <- format_snapshot_version(available_versions)
    cli::cli_abort(
      "Snapshot version [{format_snapshot_version(version)}] was not found for tag [{tag}]. Available versions: [{formatted}]."
    )
  }

  file.path(tag_dir, format_snapshot_version(version))
}

resolve_snapshot_input <- function(path, tag = NULL, version = NULL) {
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
  path_is_snapshot_version <- grepl("^v[0-9]+$", basename(path)) &&
    identical(basename(dirname(dirname(path))), "snapshots")
  if (path_is_snapshot_version) {
    cli::cli_abort(
      "Path [{path}] already points to a concrete snapshot version folder; do not combine it with {.arg tag}/{.arg version}."
    )
  }

  resolve_snapshot_version(path, tag, version, allocate = FALSE)
}

resolve_snapshot_output_folder <- function(idata,
                                           output_folder = NULL,
                                           tag = NULL,
                                           rehome = FALSE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(output_folder, len = 1, null.ok = TRUE)
  checkmate::assert_character(tag, len = 1, null.ok = TRUE)
  checkmate::assert_flag(rehome)

  provenance <- get_provenance(idata)

  if (!is.null(output_folder)) {
    return(list(
      output_folder = normalizePath(output_folder, mustWork = FALSE),
      tag = if (is.null(tag)) NULL else validate_snapshot_tag(tag),
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
  tag <- validate_snapshot_tag(tag)

  snapshot_folder <- resolve_snapshot_version(
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
