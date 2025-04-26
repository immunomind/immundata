filter_by_levenshtein <- function(uniq_seq_tbl,
                                  query_col,
                                  patterns,
                                  pattern_cols,
                                  backend = c("duckdb", "stringdist", "hybrid")) {
}


filter_by_levenshtein <- function(uniq_seq_tbl,
                                  query_col,
                                  patterns,
                                  pattern_cols,
                                  max_dist = 2,
                                  kmer_left = 3,
                                  kmer_right = 2,
                                  backend = c("duckdb", "stringdist", "hybrid")) {
  checkmate::assert_character(patterns)
  checkmate::assert_character(pattern_cols, len = length(patterns))
  checkmate::assert_integer(max_dist, lower = 1)
  checkmate::assert_integer(kmer_left, lower = 0)
  checkmate::assert_integer(kmer_left, lower = 0)
}
