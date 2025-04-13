#' Convert immunarch data to ImmunData
#'
#' Takes a typical immunarch data structure (e.g., `immdata$data` and `immdata$meta`)
#' and merges all samples into a single table. It annotates each row with
#' sample metadata, renames relevant columns, and returns a new `ImmunData` object.
#'
#' @param immdata A list-like object with fields `data` (a named list of data frames)
#'   and `meta` (a data frame of sample metadata). This is the usual format from immunarch.
#' @param sample_col A column name in `immdata$meta` that uniquely identifies each sample
#'   (default `"Sample"`).
#' @param cdr3_col The immunarch column storing CDR3 amino acid sequences (default `"CDR3.aa"`).
#' @param v_col The immunarch column storing V segment (default `"V.name"`).
#' @param j_col The immunarch column storing J segment (default `"J.name"`).
#' @param count_col The column representing read counts or clonotype counts (default `"Count"`).
#' @param schema A named list for your ImmunData receptor schema. If `NULL`, a default will be used.
#' @param verbose Logical indicating whether to print progress messages (default `FALSE`).
#'
#' @return An `ImmunData` R6 object with `.receptors` and `.annotations` tables containing
#'   the unified immunarch data.
#' @export
#' @examples
#' \dontrun{
#' # Suppose immdata is a typical immunarch dataset:
#' # immdata$data -> named list of data frames
#' # immdata$meta -> data frame with sample info
#' new_imm <- from_immunarch(immdata)
#' new_imm$receptors   # Access the receptor-level data
#' new_imm$annotations # Access the annotation-level table
#' }
from_immunarch <- function(immdata,
                           schema,
                           output_folder,
                           repertoire_id_col = "Sample",
                           cdr3_col = "CDR3.aa",
                           v_col = "V.name",
                           j_col = "J.name",
                           count_col = "Count") {

  # TODO: convert to duckplyr frames earlier
  # TODO: what to do with barcodes? Third element + list of barcodes. Some workaround?

  checkmate::assert_list(immdata, names = "named")
  checkmate::assert_list(immdata$data, any.missing = FALSE)
  checkmate::assert_data_frame(immdata$meta, null.ok = FALSE)

  # 1) Combine all sample data frames into one
  #    We'll create a "sample_id" column from the list names or from meta
  all_samples <- names(immdata$data)
  if (verbose) message("Merging ", length(all_samples), " sample data frames...")

  # For each sample, add a "sample_id" column, then bind
  combined_df <- purrr::map2_dfr(
    .x = immdata$data,
    .y = all_samples,
    ~ dplyr::mutate(.x, sample_id = .y)
  )

  # 2) Join with sample metadata (optional, if you want annotation columns from immdata$meta)
  #    We'll assume immdata$meta has a column matching 'sample_col' that lines up with .y
  #    e.g. immdata$meta$Sample
  if (!sample_col %in% colnames(immdata$meta)) {
    stop("`sample_col` not found in immdata$meta: ", sample_col)
  }
  meta_cols <- colnames(immdata$meta)

  combined_df <- combined_df %>%
    dplyr::left_join(
      immdata$meta,
      by = setNames(sample_col, "sample_id")  # e.g. by = c("sample_id" = "Sample")
    )

  # 3) Rename columns so they match your ImmunData schema
  #    We'll guess typical "cdr3", "v_call", "j_call", "count" columns
  #    so that the next steps are consistent
  rename_map <- c(
    cdr3 = cdr3_col,
    v_call = v_col,
    j_call = j_col,
    read_count = count_col
  )
  # Keep only those that exist
  rename_map <- rename_map[rename_map %in% colnames(combined_df)]

  # We'll rename old->new with new_col=old_col in dplyr::rename
  # But to avoid tidyselect warnings, use `all_of()`
  rename_map_named <- setNames(
    names(rename_map),
    unname(rename_map)
  )

  combined_df <- combined_df %>%
    dplyr::rename(dplyr::all_of(rename_map_named))

  # 4) For single-cell style data, you might define .annotations as row-level:
  #    We'll treat the full combined_df as "annotations"
  #    Then for .receptors, we can do an aggregated table or keep the same.
  #    Example: just make a minimal "receptors" by grouping cdr3, v_call, j_call
  #    and giving each a receptor_id.
  if (verbose) message("Constructing receptor-level table...")

  library(dplyr)
  receptor_df <- combined_df %>%
    dplyr::distinct(cdr3, v_call, j_call) %>%
    dplyr::mutate(imd_receptor_id = dplyr::row_number())

  # 5) Join the receptor_id back to the combined_df for linking
  annotations_df <- combined_df %>%
    dplyr::left_join(receptor_df, by = c("cdr3", "v_call", "j_call"))

  # 6) Build your schema if not provided
  if (is.null(schema)) {
    # minimal example
    schema <- list(
      # e.g. aggregator fields
      aggregate = c("cdr3", "v_call", "j_call")
    )
  }

  if (verbose) message("Creating ImmunData object...")

  # 7) Create your ImmunData
  #    If your R6 class expects .receptors / .annotations, pass them as arguments.
  new_obj <- ImmunData$new(
    receptors = receptor_df,
    annotations = annotations_df,
    schema = schema
  )

  return(new_obj)
}
