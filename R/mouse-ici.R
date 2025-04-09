# imdata = repLoad("../immundata-py/data/mouse-ici/")
#
# names(imdata$data) <- sapply(names(imdata$data), function (x) substr(stringr::str_remove_all(x, "_miXCR_clones_summary_with_PID_read_cutoff_1"), 12, 200))
#
# imdata$data2 <- lapply(1:length(imdata$data), function (df_i) { imdata$data[[df_i]][["Sample"]] <- names(imdata$data)[df_i] } )
#
# megadata = do.call(rbind, imdata$data2)
#
# readr::write_tsv(x = megadata, file = gzfile("mouse-ici.tsv.gz", compression = 9), col_names = TRUE)


# source_table <- df$receptors |>
#   head(7) |>
#   compute(name = "source_table2")
# source_table
#
# pattern_vec <- c("ASSSRDALNIQY")
# pattern_table <- duckdb_tibble(patterns = pattern_vec) |> compute(name = "pattern_table")
# pattern_table
#
# query <- "
# SELECT
#   s.imd_receptor_id,
#   s.cdr3_aa,
#   p.patterns,
#   levenshtein(s.cdr3_aa, p.patterns) AS edit_distance
# FROM source_table2 s
# CROSS JOIN pattern_table p
# WHERE edit_distance <= 7
# "

# read_sql_duckdb(query)

# query <- "
# SELECT
#   m.id,
#   m.text_column AS original_text,
#   t.target AS comparison_text,
#   levenshtein(m.text_column, t.target) AS edit_distance
# FROM main_table m
# CROSS JOIN target_strings t
# "
