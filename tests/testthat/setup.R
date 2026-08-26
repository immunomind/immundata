# Keep test-wide state consistent across test_check(), test_local(), and
# test_file(), then restore it after the complete test run.
withr::local_envvar(
  c(
    DUCKPLYR_FALLBACK_INFO = "FALSE",
    DUCKPLYR_FALLBACK_COLLECT = "0",
    DUCKPLYR_FALLBACK_AUTOUPLOAD = "0",
    DUCKPLYR_FALLBACK_VERBOSE = "FALSE"
  ),
  .local_envir = testthat::teardown_env()
)

withr::local_options(
  list(
    immundata.verbose = FALSE,
    rlib_message_verbosity = "quiet"
  ),
  .local_envir = testthat::teardown_env()
)

# Avoid nested parallelism: testthat already runs test files in parallel.
duckplyr_db_exec <- utils::getFromNamespace("db_exec", "duckplyr")
duckplyr_db_exec("SET threads TO 1")
rm(duckplyr_db_exec)
