drop_repertoire_state <- function(annotations) {
  repertoire_state_cols <- c(
    imd_schema("repertoire"),
    imd_schema("strata"),
    imd_schema("strata_name"),
    imd_schema("count"),
    imd_schema("proportion"),
    imd_schema("n_receptors"),
    imd_schema("n_barcodes"),
    imd_schema("n_repertoires")
  )

  annotations |> select(-any_of(repertoire_state_cols))
}

rebuild_repertoire_and_strata <- function(idata, source_idata) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_r6(source_idata, "ImmunData")

  rebuilt <- agg_repertoires(idata, source_idata$schema_repertoire)

  if (is.null(source_idata$schema_strata) || is.null(source_idata$strata)) {
    return(rebuilt)
  }

  strata_col <- imd_schema("strata")
  strata_name_col <- imd_schema("strata_name")
  rebuilt <- rebuilt |> agg_strata(source_idata$schema_strata)

  old_strata_labels <- source_idata$strata |>
    duckplyr::as_duckdb_tibble() |>
    select(all_of(c(source_idata$schema_strata, strata_name_col)))
  rebuilt_strata_labels <- rebuilt$strata |>
    duckplyr::as_duckdb_tibble() |>
    select(all_of(c(strata_col, source_idata$schema_strata))) |>
    left_join(
      old_strata_labels,
      by = source_idata$schema_strata,
      na_matches = "na"
    ) |>
    collect()

  strata_names <- rebuilt_strata_labels[[strata_name_col]]
  if (all(!is.na(strata_names))) {
    rebuilt <- rename_strata(
      rebuilt,
      names = stats::setNames(
        as.character(strata_names),
        as.character(rebuilt_strata_labels[[strata_col]])
      )
    )
  }

  rebuilt
}
