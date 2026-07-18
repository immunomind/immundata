get_test_idata_tsv_no_manifest <- function(schema = c("cdr3_aa", "v_call"),
                                           repertoire_schema = "<auto>") {
  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )
  read_repertoires(
    path          = sample_files,
    schema        = schema,
    repertoire_schema = repertoire_schema,
    output_folder = tempfile()
  )
}

get_test_idata_tsv_with_manifest <- function(schema = c("cdr3_aa", "v_call"),
                                             repertoire_schema = "<auto>") {
  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")
  manifest <- read_manifest(manifest_path)

  sample_files <- c(
    system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"),
    system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )
  read_repertoires(
    path = sample_files,
    schema = schema,
    manifest = manifest,
    repertoire_schema = repertoire_schema,
    output_folder = tempfile()
  )
}

get_test_ig_drop_cols <- function() {
  c(
    "productive",
    "rev_comp",
    "d_call",
    "junction_length",
    "junction_aa_length",
    "v_sequence_start",
    "v_sequence_end",
    "d_sequence_start",
    "d_sequence_end",
    "j_sequence_start",
    "j_sequence_end",
    "c_sequence_start",
    "c_sequence_end",
    "consensus_count",
    "is_cell"
  )
}

get_test_ig_data <- function(
    mode = c("single", "paired", "relaxed"),
    features = c("v_call", "j_call", "junction_aa"),
    output_folder = tempfile(),
    preprocess = list(
      exclude_columns = make_exclude_columns(cols = get_test_ig_drop_cols())
    ),
    postprocess = NULL,
    rename_columns = NULL) {
  mode <- match.arg(mode)

  schema <- switch(
    mode,
    single = make_receptor_schema(
      features = features,
      chains = "IGH"
    ),
    paired = make_receptor_schema(
      features = features,
      chains = c("IGH", "IGL")
    ),
    relaxed = make_receptor_schema(
      features = features,
      chains = c("IGH", "IGL|IGK")
    )
  )

  read_repertoires(
    path = system.file("extdata/ig", "multiple_ig_loci.tsv.gz", package = "immundata"),
    schema = schema,
    barcode_col = "cell_id",
    locus_col = "locus",
    umi_col = "duplicate_count",
    output_folder = output_folder,
    preprocess = preprocess,
    postprocess = postprocess,
    rename_columns = rename_columns
  )
}

load_test_ig_idata <- get_test_ig_data

#' Get test datasets from `immundata`
#' @keywords internal
#' @export
get_test_idata <- function() {
  get_test_idata_tsv_with_manifest()
}

#' Get test datasets from `immundata`
#' @keywords internal
#' @export
get_test_immundata <- function(repertoire_schema = "<auto>") {
  get_test_idata_tsv_with_manifest(repertoire_schema = repertoire_schema)
}

get_test_idata_tsv_manifest <- function() {
  manifest_path <- system.file("extdata/tsv", "manifest.csv", package = "immundata")

  read_manifest(manifest_path)
}
