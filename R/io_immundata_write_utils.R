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

build_write_metadata_json <- function(idata,
                                      producer_function,
                                      metadata_lineage_inputs = NULL,
                                      metadata_lineage_args = NULL,
                                      metadata_lineage_columns = NULL,
                                      metadata_lineage_pipeline = NULL,
                                      metadata_extensions = NULL) {
  metadata_json <- list(
    format_version = 2L,
    package_version = as.character(packageVersion("immundata")),
    schema_receptor = idata$schema_receptor,
    schema_repertoire = idata$schema_repertoire,
    producer = list("function" = producer_function),
    extensions = validate_metadata_extensions(metadata_extensions)
  )

  lineage <- build_metadata_lineage(
    metadata_lineage_inputs = metadata_lineage_inputs,
    metadata_lineage_args = metadata_lineage_args,
    metadata_lineage_columns = metadata_lineage_columns,
    metadata_lineage_pipeline = metadata_lineage_pipeline
  )

  if (!is.null(lineage)) {
    metadata_json$lineage <- lineage
  }

  metadata_json
}
