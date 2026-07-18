#' @title Downsample ImmunData annotations at the repertoire level
#'
#' @description
#' Downsamples an `ImmunData` object by selecting barcodes within each repertoire.
#' If repertoire schema is not defined, the entire dataset is treated as one repertoire.
#'
#' The function uses a single parameter `n`:
#' - If `0 < n < 1`, `n` is treated as a proportion of repertoire size.
#' - If `n >= 1`, `n` is treated as an absolute target count. In particular,
#'   `n = 1` retains one barcode/receptor unit per repertoire for cell-level
#'   data, or one count per repertoire for bulk data.
#'
#' Downsampling is barcode-based. For count-based inputs (e.g. bulk),
#' per-barcode counts (`imd_n_chains`) are trimmed when needed to reach the target.
#' Existing repertoire-derived metrics (`imd_count`, `imd_proportion`,
#' `n_repertoires`, `n_receptors`, `n_barcodes`) are dropped and recalculated
#' for the downsampled object when a repertoire schema is present. Original
#' metrics are not retained because they describe the pre-downsampled universe.
#' If the input has a strata schema, the strata layer is rebuilt from that
#' schema after repertoire aggregation. Existing strata labels are retained.
#'
#' @param idata An `ImmunData` object.
#' @param n Numeric scalar controlling downsampling amount.
#'   See details for interpretation.
#' @param seed Optional integer scalar for reproducible sampling.
#'
#' @return A new `ImmunData` object with downsampled annotations.
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
  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")

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
      do.call(order, unit_table[unit_cols]),
      ,
      drop = FALSE
    ]
  }

  group_ids <- if (has_repertoire) {
    as.character(unit_table[[repertoire_col]])
  } else {
    rep("__all__", nrow(unit_table))
  }

  draw_weighted <- function(weights, size) {
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

    sampled_counts <- draw_weighted(group_df[[count_col]], target)
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
    provenance = imd_get_provenance(idata)
  )

  if (is.null(idata$schema_repertoire)) {
    return(new_idata)
  }

  downsampled <- new_idata |> agg_repertoires(idata$schema_repertoire)

  if (is.null(idata$schema_strata) || is.null(idata$stratas)) {
    return(downsampled)
  }

  rebuilt_strata <- downsampled |> agg_strata(idata$schema_strata)

  old_strata_labels <- idata$stratas |>
    select(all_of(c(idata$schema_strata, strata_name_col)))
  rebuilt_strata_labels <- rebuilt_strata$stratas |>
    select(all_of(c(strata_col, idata$schema_strata))) |>
    left_join(old_strata_labels, by = idata$schema_strata, na_matches = "na")

  strata_names <- rebuilt_strata_labels[[strata_name_col]]
  if (all(!is.na(strata_names))) {
    rebuilt_strata <- rename_strata(
      rebuilt_strata,
      names = stats::setNames(
        as.character(strata_names),
        as.character(rebuilt_strata_labels[[strata_col]])
      )
    )
  }

  rebuilt_strata
}
