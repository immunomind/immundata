# imdata = repLoad("../immundata-py/data/mouse-ici/")
#
# names(imdata$data) <- sapply(names(imdata$data), function (x) substr(stringr::str_remove_all(x, "_miXCR_clones_summary_with_PID_read_cutoff_1"), 12, 200))
#
# imdata$data2 <- lapply(1:length(imdata$data), function (df_i) { imdata$data[[df_i]][["Sample"]] <- names(imdata$data)[df_i] } )
#
# megadata = do.call(rbind, imdata$data2)
#
# readr::write_tsv(x = megadata, file = gzfile("mouse-ici.tsv.gz", compression = 9), col_names = TRUE)
