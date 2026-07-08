# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)

Sys.setenv(
  DUCKPLYR_FALLBACK_INFO = "FALSE",
  DUCKPLYR_FALLBACK_COLLECT = "0",
  DUCKPLYR_FALLBACK_AUTOUPLOAD = "0",
  DUCKPLYR_FALLBACK_VERBOSE = "FALSE"
)
options(rlib_message_verbosity = "quiet")

library(immundata)

test_check("immundata")
