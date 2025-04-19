#' @concept Annotation
#' @rdname annotate
#' @export
annotate_immundata <- function(idata,
                               annotations,
                               match_col = c(
                                 idata = imd_schema()$receptor,
                                 annotations = "receptor_id"
                               ), remove_limit = FALSE) {
  checkmate::assert_r6(idata, "ImmunData")
  checkmate::assert_data_frame(annotations)
  checkmate::assert_character(match_col, len = 2, names = "named")

  idata_col <- match_col["idata"]
  annot_col <- match_col["annotations"]

  ann_tbl <- as_tibble(annotations)

  if (!annot_col %in% colnames(ann_tbl)) {
    cli_abort("Column '{annot_col}' not found in annotations.")
  }
  ann_tbl <- ann_tbl |>
    rename({{ idata_col }} := !!rlang::sym(annot_col)) |>
    as_duckdb_tibble()
  print(colnames(ann_tbl))
  print(colnames(idata$annotations))
  print(ann_tbl)
  cli::cli_h1("Whoa")

  new_annotations <- idata$annotations %>%
    left_join(ann_tbl, by = idata_col)

  ImmunData$new(
    receptors   = idata$receptors,
    annotations = new_annotations,
    schema      = idata$schema_receptor
  )
}

#' @concept Annotation
#' @rdname annotate
#' @export
annotate_receptors <- function(idata,
                               annotations,
                               annot_col = "sequence_id",
                               remove_limit = FALSE) {
  match_col <- c()
  annotate_immundata(idata = idata, annotations = annotations, match_col = match_col, remove_limit = remove_limit)
  #   checkmate::assert_r6(idata, "ImmunData")
  #   checkmate::assert_data_frame(annotations)
  #   checkmate::assert_character(match_col, len = 2, names = "named")
  #   checkmate::assert_logical(remove_limit, len = 1)
  #
  #   idata_col <- match_col["idata"]
  #   annot_col <- match_col["annotations"]
  #
  #   ann_tbl <- as_tibble(annotations)
  #
  #   # if user wants to use rownames of the data.frame, e.g., in case of Seurat meta.data
  #   if (annot_col == "<rownames>") {
  #     ann_tbl[[idata_col]] <- rownames(annotations)
  #   } else {
  #     if (!annot_col %in% colnames(ann_tbl)) {
  #       cli_abort("Column '{annot_col}' not found in annotations.")
  #     }
  #     ann_tbl[[idata_col]] <- ann_tbl[[annot_col]]
  #   }
  #
  #   # optionally guard against huge annotation tables
  #   if (!remove_limit && ncol(ann_tbl) > 50) {
  #     cli_abort("Well, well, well, would you look at that... Decided to dump all tens of thousands genes into your repertoire data?
  # I mean, sure, do whataver you want. But I'm not responsible for the freezes or crashes.
  # Pass cli::col_magenta('`remove_limit = True`') to `annotate_cells` to allow working with annotations of arbitrary size.
  # But remember: cli::col_red('you have been warned').")
  #   }
  #
  #   ann_tbl <- ann_tbl %>%
  #     select(-any_of(annot_col), all_of(idata_col), everything()) |>
  #     as_duckdb_tibble()
  #
  #   new_annotations <- idata$annotations %>%
  #     left_join(ann_tbl, by = idata_col)
  #
  #   ImmunData$new(
  #     receptors   = idata$receptors,
  #     annotations = new_annotations,
  #     schema      = idata$schema_receptor
  #   )
}

#' @concept Annotation
#' @rdname annotate
#' @export
annotate_cells <- function(idata,
                           annotations,
                           annot_col = "<rownames>",
                           remove_limit = FALSE) {
  match_col <- c()
  annotate_immundata(idata = idata, annotations = annotations, match_col = match_col, remove_limit = remove_limit)
}
