generate_snapshot_id <- function() {
  suffix <- paste0(sample(c(letters, 0:9), 8L, replace = TRUE), collapse = "")
  paste0("imd_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_", suffix)
}

build_snapshot_metadata <- function(idata,
                                    producer_function,
                                    provenance_before,
                                    output_folder,
                                    snapshot_tag = NULL,
                                    rehome = FALSE,
                                    ingestion_payload = NULL,
                                    metadata_extensions = NULL) {
  serialized_repertoires <- idata$repertoires
  if (!is.null(serialized_repertoires)) {
    checkmate::assert_data_frame(serialized_repertoires)
    serialized_repertoires <- as.list(serialized_repertoires)
    factor_columns <- vapply(serialized_repertoires, is.factor, logical(1))
    serialized_repertoires[factor_columns] <- lapply(
      serialized_repertoires[factor_columns],
      as.character
    )
  }

  snapshot_id <- generate_snapshot_id()
  is_ingestion <- identical(producer_function, "read_repertoires")
  event <- list(
    event = if (is_ingestion) "ingestion" else "snapshot",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
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
      repertoires = serialized_repertoires,
      producer = list("function" = producer_function),
      snapshot_id = snapshot_id,
      lineage = lineage,
      provenance = provenance_paths_for_metadata(provenance_after),
      extensions = if (is.null(metadata_extensions)) list() else metadata_extensions
    ),
    provenance = provenance_after
  )
}
