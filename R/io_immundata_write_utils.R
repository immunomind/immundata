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

normalize_provenance <- function(provenance = NULL,
                                 fallback_home_path = NULL,
                                 fallback_current_path = NULL,
                                 fallback_snapshot_id = NULL,
                                 fallback_lineage = NULL) {
  if (is.null(provenance)) {
    provenance <- list()
  }

  checkmate::assert_list(provenance)
  defaults <- imd_default_provenance()

  if (!is.null(provenance$home_path)) {
    defaults$home_path <- provenance$home_path
  }
  if (!is.null(provenance$current_path)) {
    defaults$current_path <- provenance$current_path
  }
  if (!is.null(provenance$snapshot_root)) {
    defaults$snapshot_root <- provenance$snapshot_root
  }
  if (!is.null(provenance$snapshot_id)) {
    defaults$snapshot_id <- provenance$snapshot_id
  }
  if (!is.null(provenance$lineage)) {
    defaults$lineage <- provenance$lineage
  }

  if (is.null(defaults$home_path) && !is.null(fallback_home_path)) {
    defaults$home_path <- fallback_home_path
  }
  if (is.null(defaults$current_path) && !is.null(fallback_current_path)) {
    defaults$current_path <- fallback_current_path
  }
  if (is.null(defaults$snapshot_id) && !is.null(fallback_snapshot_id)) {
    defaults$snapshot_id <- fallback_snapshot_id
  }
  if (length(defaults$lineage) == 0 && !is.null(fallback_lineage)) {
    defaults$lineage <- fallback_lineage
  }

  defaults$home_path <- normalize_nullable_path(defaults$home_path)
  defaults$current_path <- normalize_nullable_path(defaults$current_path)
  defaults$snapshot_root <- normalize_nullable_path(defaults$snapshot_root)
  if (is.null(defaults$snapshot_root) && !is.null(defaults$home_path)) {
    defaults$snapshot_root <- normalizePath(file.path(defaults$home_path, "snapshots"), mustWork = FALSE)
  }

  if (is.null(defaults$snapshot_id)) {
    defaults$snapshot_id <- imd_generate_snapshot_id()
  }
  checkmate::assert_character(defaults$snapshot_id, len = 1, null.ok = FALSE)

  checkmate::assert_list(defaults$lineage)
  for (event in defaults$lineage) {
    checkmate::assert_list(event)
  }

  defaults
}

imd_get_provenance <- function(idata) {
  checkmate::assert_r6(idata, "ImmunData")
  private_env <- idata$.__enclos_env__$private
  raw <- private_env$.provenance
  normalize_provenance(raw)
}

imd_set_provenance <- function(idata, provenance) {
  checkmate::assert_r6(idata, "ImmunData")
  normalized <- normalize_provenance(provenance)
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

  snapshot_root <- provenance$snapshot_root
  if (is.null(snapshot_root)) {
    snapshot_root <- normalizePath(file.path(provenance$home_path, "snapshots"), mustWork = FALSE)
  }

  tag_dir <- file.path(snapshot_root, tag)
  dir.create(tag_dir, recursive = TRUE, showWarnings = FALSE)

  next_version <- imd_next_snapshot_version(tag_dir)
  snapshot_folder <- file.path(tag_dir, imd_format_snapshot_version(next_version))

  list(
    output_folder = normalizePath(snapshot_folder, mustWork = FALSE),
    tag = tag,
    provenance = provenance,
    output_was_auto = TRUE
  )
}

validate_metadata_lineage_inputs <- function(metadata_lineage_inputs) {
  checkmate::assert_list(metadata_lineage_inputs)

  required_fields <- c("files", "metadata_joined", "enforce_schema")
  checkmate::assert_names(
    names(metadata_lineage_inputs),
    must.include = required_fields,
    subset.of = required_fields
  )

  checkmate::assert_character(metadata_lineage_inputs$files, min.len = 1)
  checkmate::assert_logical(metadata_lineage_inputs$metadata_joined, len = 1)
  checkmate::assert_logical(metadata_lineage_inputs$enforce_schema, len = 1)

  metadata_lineage_inputs
}

validate_metadata_lineage_args <- function(metadata_lineage_args) {
  checkmate::assert_list(metadata_lineage_args)

  required_fields <- c("barcode_col", "count_col", "locus_col", "umi_col", "metadata_file_col")
  checkmate::assert_names(
    names(metadata_lineage_args),
    must.include = required_fields,
    subset.of = required_fields
  )

  checkmate::assert_character(metadata_lineage_args$barcode_col, max.len = 1, null.ok = TRUE)
  checkmate::assert_character(metadata_lineage_args$count_col, max.len = 1, null.ok = TRUE)
  checkmate::assert_character(metadata_lineage_args$locus_col, max.len = 1, null.ok = TRUE)
  checkmate::assert_character(metadata_lineage_args$umi_col, max.len = 1, null.ok = TRUE)
  checkmate::assert_character(metadata_lineage_args$metadata_file_col, len = 1, null.ok = FALSE)

  metadata_lineage_args
}

validate_metadata_lineage_columns <- function(metadata_lineage_columns) {
  checkmate::assert_list(metadata_lineage_columns)

  required_top_fields <- c("renamed", "dropped")
  checkmate::assert_names(
    names(metadata_lineage_columns),
    must.include = required_top_fields,
    subset.of = required_top_fields
  )

  renamed <- metadata_lineage_columns$renamed
  checkmate::assert_list(renamed)
  checkmate::assert_names(
    names(renamed),
    must.include = c("requested", "applied", "not_found"),
    subset.of = c("requested", "applied", "not_found")
  )
  checkmate::assert_character(renamed$requested, null.ok = TRUE)
  checkmate::assert_character(renamed$applied, null.ok = TRUE)
  checkmate::assert_character(renamed$not_found, null.ok = TRUE)

  dropped <- metadata_lineage_columns$dropped
  checkmate::assert_list(dropped)
  checkmate::assert_names(
    names(dropped),
    must.include = c("applied"),
    subset.of = c("applied")
  )
  checkmate::assert_character(dropped$applied, null.ok = TRUE)

  metadata_lineage_columns
}

validate_metadata_lineage_pipeline <- function(metadata_lineage_pipeline) {
  checkmate::assert_list(metadata_lineage_pipeline)

  required_fields <- c("preprocess", "postprocess")
  checkmate::assert_names(
    names(metadata_lineage_pipeline),
    must.include = required_fields,
    subset.of = required_fields
  )

  checkmate::assert_character(metadata_lineage_pipeline$preprocess, null.ok = TRUE)
  checkmate::assert_character(metadata_lineage_pipeline$postprocess, null.ok = TRUE)

  metadata_lineage_pipeline
}

validate_metadata_extensions <- function(metadata_extensions) {
  if (is.null(metadata_extensions)) {
    return(list())
  }

  checkmate::assert_list(metadata_extensions)
  if (!is.null(names(metadata_extensions))) {
    checkmate::assert_true(all(names(metadata_extensions) != ""))
  }

  metadata_extensions
}

build_metadata_lineage <- function(metadata_lineage_inputs = NULL,
                                   metadata_lineage_args = NULL,
                                   metadata_lineage_columns = NULL,
                                   metadata_lineage_pipeline = NULL) {
  lineage_fields <- c(
    !is.null(metadata_lineage_inputs),
    !is.null(metadata_lineage_args),
    !is.null(metadata_lineage_columns),
    !is.null(metadata_lineage_pipeline)
  )

  if (any(lineage_fields) && !all(lineage_fields)) {
    cli::cli_abort(
      "Lineage metadata must be passed as a complete set: inputs, args, columns, and pipeline."
    )
  }

  if (!any(lineage_fields)) {
    return(NULL)
  }

  list(
    inputs = validate_metadata_lineage_inputs(metadata_lineage_inputs),
    args = validate_metadata_lineage_args(metadata_lineage_args),
    column_lineage = validate_metadata_lineage_columns(metadata_lineage_columns),
    pipeline = validate_metadata_lineage_pipeline(metadata_lineage_pipeline)
  )
}

validate_lineage_events <- function(lineage) {
  if (is.null(lineage)) {
    return(list())
  }

  checkmate::assert_list(lineage)
  for (event in lineage) {
    checkmate::assert_list(event)
  }

  lineage
}

build_lineage_event <- function(event,
                                producer_function,
                                snapshot_id,
                                ingestion_payload = NULL,
                                source_path = NULL,
                                snapshot_path = NULL,
                                tag = NULL) {
  checkmate::assert_choice(event, c("ingestion", "snapshot"))
  checkmate::assert_character(producer_function, len = 1, null.ok = FALSE)
  checkmate::assert_character(snapshot_id, len = 1, null.ok = FALSE)
  checkmate::assert_character(source_path, len = 1, null.ok = TRUE)
  checkmate::assert_character(snapshot_path, len = 1, null.ok = TRUE)
  checkmate::assert_character(tag, len = 1, null.ok = TRUE)

  out <- list(
    event = event,
    created_at = imd_now_utc_iso(),
    snapshot_id = snapshot_id,
    producer = list("function" = producer_function)
  )

  if (event == "ingestion") {
    if (is.null(ingestion_payload)) {
      cli::cli_abort("Ingestion lineage event requires ingestion payload.")
    }

    out$inputs <- ingestion_payload$inputs
    out$args <- ingestion_payload$args
    out$column_lineage <- ingestion_payload$column_lineage
    out$pipeline <- ingestion_payload$pipeline
    return(out)
  }

  out$source_path <- source_path
  out$snapshot_path <- snapshot_path
  out$tag <- tag
  out
}

build_write_metadata_json <- function(idata,
                                      producer_function,
                                      snapshot_id,
                                      lineage,
                                      provenance,
                                      metadata_extensions = NULL) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_character(producer_function, len = 1, null.ok = FALSE)
  checkmate::assert_character(snapshot_id, len = 1, null.ok = FALSE)

  metadata_json <- list(
    format_version = 2L,
    package_version = as.character(packageVersion("immundata")),
    schema_receptor = idata$schema_receptor,
    schema_repertoire = idata$schema_repertoire,
    producer = list("function" = producer_function),
    snapshot_id = snapshot_id,
    lineage = validate_lineage_events(lineage),
    provenance = normalize_provenance(provenance),
    extensions = validate_metadata_extensions(metadata_extensions)
  )

  metadata_json
}
