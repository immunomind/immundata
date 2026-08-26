# Avoid nested parallelism: testthat already runs test files in parallel.
duckplyr_db_exec <- utils::getFromNamespace("db_exec", "duckplyr")
duckplyr_db_exec("SET threads TO 1")
rm(duckplyr_db_exec)
