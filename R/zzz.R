.onLoad <- function(libname, pkgname) {
  # has_duckplyr <- requireNamespace("duckplyr", quietly = TRUE)
  # has_duckdb <- requireNamespace("duckdb", quietly = TRUE)
  # duckdb_ge_150 <- has_duckdb &&
  #   utils::packageVersion("duckdb") >= base::package_version("1.5.0")
  #
  # if (has_duckplyr && duckdb_ge_150) {
  #   try(
  #     duckplyr::db_exec("SET disabled_optimizers = 'top_n_window_elimination'"),
  #     silent = TRUE
  #   )
  # }
}
