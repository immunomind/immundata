#' @title Reduce repertoires to a common sampling depth
#'
#' @description
#' Use `downsample_immundata()` to reduce every repertoire to the same number or
#' fraction of observed cells or bulk sequence counts before comparing
#' repertoires. So, it is just a downsampling.
#'
#' Use this function when different sequencing depths could affect a comparison
#' of repertoire diversity or composition. In single-cell data, the sampling
#' unit is a cell barcode and all selected chains from that cell stay together.
#' In bulk data with abundance values, the sampling unit is one sequence count.
#'
#' The function returns a new [ImmunData] object. The original object is not
#' changed.
#'
#' @section Meaning of `n` for single-cell data:
#'
#' * `0 < n < 1` keeps `floor(n * number of cells)` cells from each repertoire.
#' * `n >= 1` keeps `n` cells from each repertoire.
#'
#' Cell barcodes are sampled without replacement. For paired receptors, all
#' retained chains belonging to a selected cell stay together.
#'
#' @section Meaning of `n` for bulk data:
#'
#' * `0 < n < 1` keeps `floor(n * total abundance)` sequence counts from each
#'   repertoire.
#' * `n >= 1` keeps a total abundance of `n` from each repertoire.
#'
#' Counts are sampled without replacement according to their observed
#' abundance. A retained receptor can therefore have a smaller abundance than
#' it had before downsampling. For example, `n = 1000` makes the total retained
#' abundance equal to 1000 in every repertoire that originally contained at
#' least 1000 counts.
#'
#' If a requested whole-number `n` is larger than a repertoire, that repertoire
#' is returned unchanged and the function gives a warning. If a fraction is so
#' small that it selects zero units in any repertoire, the function stops and
#' asks for a larger value.
#'
#' @section Repertoire and strata summaries:
#'
#' When repertoires are defined, the function recalculates receptor counts,
#' proportions, repertoire sizes, and the number of repertoires containing each
#' receptor. Existing strata are also rebuilt, and their labels are retained.
#' When repertoires are not defined, the complete dataset is treated as one
#' sampling group and no repertoire summary is added.
#'
#' @section Backend and storage:
#'
#' Chain-level selection and reconstruction use the duckplyr annotation table.
#' The small table of sampling units is collected into R for random sampling.
#' The function does not overwrite the stored input object. Use
#' [write_immundata()] to save the returned object.
#'
#' @param idata An [ImmunData] object. For comparisons between repertoires, its
#'   repertoires should already be defined with [read_repertoires()] or
#'   [agg_repertoires()].
#' @param n A number. Sampling depth. Use a value strictly between 0 and 1 for a
#'   fraction, or a whole number greater than or equal to 1 for an absolute
#'   number of cells or bulk sequence counts.
#' @param seed A non-negative integer or `NULL`. Used to reproduce the same
#'   random sample. The default is `NULL`.
#'
#' @return A new [ImmunData] object containing the sampled chain observations.
#'   If the input has repertoires or strata, their summaries are recalculated
#'   for the sampled data.
#'
#' @seealso [agg_repertoires()], [filter_immundata()], [write_immundata()]
#'
#' @examples
#' library(immundata)
#' library(dplyr)
#'
#' options(immundata.verbose = FALSE)
#'
#' # Create two small bulk T-cell repertoires with different total abundances.
#' bulk_data <- tibble(
#'   Sample = c("Tumor", "Tumor", "Blood", "Blood"),
#'   cdr3_aa = c("CASSA", "CASSB", "CASSA", "CASSC"),
#'   v_call = c("TRBV1", "TRBV2", "TRBV1", "TRBV3"),
#'   abundance = c(20L, 5L, 4L, 6L)
#' )
#' bulk_file <- tempfile(fileext = ".tsv")
#' readr::write_tsv(bulk_data, bulk_file)
#'
#' idata <- read_repertoires(
#'   path = bulk_file,
#'   schema = c("cdr3_aa", "v_call"),
#'   count_col = "abundance",
#'   repertoire_schema = "Sample",
#'   preprocess = NULL,
#'   postprocess = NULL,
#'   rename_columns = NULL,
#'   output_folder = tempfile("immundata-downsample-")
#' )
#'
#' before <- idata$repertoires |>
#'   select(Sample, n_barcodes) |>
#'   rename(before = n_barcodes)
#'
#' sampled <- downsample_immundata(idata, n = 5, seed = 42)
#'
#' before |>
#'   left_join(
#'     sampled$repertoires |>
#'       select(Sample, n_barcodes) |>
#'       rename(after = n_barcodes),
#'     by = "Sample"
#'   ) |>
#'   arrange(Sample)
#' # Expected result:
#' #   Sample before after
#' #   Blood      10     5
#' #   Tumor      25     5
#'
#' # Each returned repertoire has five sequence counts. `idata` still has its
#' # original repertoire sizes of 10 and 25.
#'
#' @concept filtering
#' @export
downsample_immundata <- function(idata, n, seed = NULL) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_number(n, lower = 0, finite = TRUE)
  checkmate::assert_integerish(seed, len = 1, null.ok = TRUE, lower = 0)

  if (n > 1 && abs(n - round(n)) > sqrt(.Machine$double.eps)) {
    cli::cli_abort("When `n > 1`, `n` must be an integer count.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  receptor_col <- imd_schema("receptor")
  barcode_col <- imd_schema("barcode")
  chain_count_col <- imd_schema("chain_count")
  count_col <- imd_schema("count")
  repertoire_col <- imd_schema("repertoire")
  prop_col <- imd_schema("proportion")
  n_repertoires_col <- imd_schema("n_repertoires")
  n_receptors_col <- imd_schema("n_receptors")
  n_barcodes_col <- imd_schema("n_barcodes")

  annotations_base <- idata$annotations |>
    select(-any_of(c(count_col, prop_col, n_repertoires_col, n_receptors_col, n_barcodes_col)))

  has_repertoire <- !is.null(idata$schema_repertoire)
  if (has_repertoire && !(repertoire_col %in% colnames(annotations_base))) {
    cli::cli_abort(
      "Repertoire aggregation is incomplete: {.field {repertoire_col}} is missing from {.field idata$annotations}."
    )
  }
  unit_cols <- c(if (has_repertoire) repertoire_col else character(0), barcode_col)

  is_count_mode <- annotations_base |>
    summarise(
      any_non_unit = any(!!rlang::sym(chain_count_col) != 1)
    ) |>
    collect() |>
    pull("any_non_unit")

  unit_base <- annotations_base |>
    select(all_of(unique(c(unit_cols, receptor_col, chain_count_col)))) |>
    summarise(
      .by = all_of(c(unit_cols, receptor_col)),
      !!chain_count_col := dplyr::first(!!rlang::sym(chain_count_col))
    )

  unit_table <- if (is_count_mode) {
    unit_base |>
      summarise(
        .by = all_of(unit_cols),
        !!count_col := sum(!!rlang::sym(chain_count_col))
      ) |>
      collect()
  } else {
    unit_base |>
      summarise(
        .by = all_of(unit_cols),
        !!count_col := 1L
      ) |>
      collect()
  }

  if (nrow(unit_table) == 0) {
    cli::cli_abort("No barcode units available for downsampling.")
  }

  if (!is.null(seed)) {
    # Keep deterministic sampling order across lazy backend materialization.
    unit_table <- unit_table[
      do.call(order, unit_table[unit_cols]), ,
      drop = FALSE
    ]
  }

  group_ids <- if (has_repertoire) {
    as.character(unit_table[[repertoire_col]])
  } else {
    rep("__all__", nrow(unit_table))
  }

  split_groups <- split(unit_table, group_ids)
  sampled_units_list <- vector("list", length(split_groups))
  n_clipped <- 0L

  for (i in seq_along(split_groups)) {
    group_df <- split_groups[[i]]
    total_count <- sum(group_df[[count_col]])
    target_raw <- if (n < 1) floor(total_count * n) else as.integer(round(n))
    target <- min(as.integer(target_raw), as.integer(total_count))

    if (target_raw > total_count) {
      n_clipped <- n_clipped + 1L
    }

    if (target < 1) {
      cli::cli_abort("No barcode units were selected. Increase `n`.")
    }

    if (!is_count_mode) {
      sampled_units_list[[i]] <- group_df[sample.int(nrow(group_df), size = target, replace = FALSE), , drop = FALSE]
      next
    }

    if (target >= total_count) {
      sampled_units_list[[i]] <- group_df
      next
    }

    sampled_counts <- draw_weighted_counts(group_df[[count_col]], target)
    out <- group_df[sampled_counts > 0, , drop = FALSE]
    out[[count_col]] <- sampled_counts[sampled_counts > 0]
    sampled_units_list[[i]] <- out
  }

  if (n_clipped > 0L && n > 1) {
    cli::cli_warn("Requested `n` exceeds available units in {n_clipped} repertoire(s). Those repertoires were returned unchanged.")
  }

  sampled_units <- dplyr::bind_rows(sampled_units_list)

  if (nrow(sampled_units) == 0) {
    cli::cli_abort("No barcode units were selected. Increase `n`.")
  }

  if (!is_count_mode) {
    sampled_keys <- duckdb_tibble(sampled_units |>
      select(all_of(unit_cols)))
    new_annotations <- annotations_base |>
      semi_join(sampled_keys, by = unit_cols)
  } else {
    n_duplicate_units <- annotations_base |>
      summarise(
        .by = all_of(unit_cols),
        n_rows = n()
      ) |>
      filter(.data$n_rows > 1) |>
      summarise(n_dups = n()) |>
      collect() |>
      pull("n_dups")

    if (length(n_duplicate_units) == 0) {
      n_duplicate_units <- 0
    }

    if (n_duplicate_units > 0) {
      cli::cli_warn("Detected duplicated unit rows in count mode ({n_duplicate_units}). Collapsing to one row per unit before downsampling join.")
    }

    sampled_units_chain <- sampled_units |>
      select(all_of(c(unit_cols, count_col)))
    colnames(sampled_units_chain)[colnames(sampled_units_chain) == count_col] <- chain_count_col

    sampled_tbl <- duckdb_tibble(sampled_units_chain)
    unit_annotation_cols <- setdiff(colnames(annotations_base), c(unit_cols, chain_count_col))

    unit_annotations <- annotations_base |>
      select(-all_of(chain_count_col)) |>
      summarise(
        .by = all_of(unit_cols),
        dplyr::across(all_of(unit_annotation_cols), dplyr::first)
      )

    new_annotations <- unit_annotations |>
      inner_join(sampled_tbl, by = unit_cols)
  }

  new_idata <- ImmunData$new(
    schema = idata$schema_receptor,
    annotations = new_annotations,
    provenance = get_provenance(idata)
  )

  if (is.null(idata$schema_repertoire)) {
    return(new_idata)
  }

  rebuild_repertoire_and_strata(new_idata, idata)
}

draw_weighted_counts <- function(weights, size) {
  weights <- as.integer(round(weights))
  out <- integer(length(weights))
  total <- sum(weights)

  if (size <= 0 || total <= 0) {
    return(out)
  }

  if (size >= total) {
    return(weights)
  }

  remaining_draws <- as.integer(size)
  remaining_total <- as.integer(total)

  if (length(weights) == 1) {
    out[1] <- remaining_draws
    return(out)
  }

  # Exact weighted sampling without replacement via sequential hypergeometric draws.
  for (i in seq_len(length(weights) - 1)) {
    wi <- as.integer(weights[i])

    if (wi <= 0 || remaining_draws <= 0) {
      out[i] <- 0L
    } else {
      out[i] <- as.integer(
        stats::rhyper(
          nn = 1,
          m = wi,
          n = remaining_total - wi,
          k = remaining_draws
        )
      )
      remaining_draws <- remaining_draws - out[i]
    }

    remaining_total <- remaining_total - wi
  }

  out[length(weights)] <- remaining_draws
  out
}
