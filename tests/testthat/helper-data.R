make_bulk_count_test_data <- function() {
  tibble::tibble(
    sample_id = c("S1", "S1", "S2", "S2"),
    v_call = c("TRBV1", "TRBV2", "TRBV3", "TRBV4"),
    j_call = c("TRBJ1", "TRBJ2", "TRBJ1", "TRBJ2"),
    junction_aa = c("AAAA", "BBBB", "CCCC", "DDDD"),
    clone_count = c(8L, 7L, 6L, 9L)
  )
}

make_single_cell_downsample_test_data <- function() {
  tibble::tibble(
    cell_id = paste0("c", seq_len(8L)),
    sample_id = rep(c("S1", "S2"), each = 4L),
    v_call = rep(paste0("IGHV", seq_len(4L)), 2L),
    j_call = rep(paste0("IGHJ", seq_len(4L)), 2L),
    junction_aa = paste0("CAR", LETTERS[seq_len(8L)]),
    locus = "IGH",
    umi_count = 10:17
  )
}

make_duplicate_chain_test_data <- function(tied = FALSE) {
  umi_count <- if (tied) {
    c(100L, 100L, 80L, 80L)
  } else {
    c(100L, 150L, 80L, 60L)
  }

  tibble::tibble(
    cell_id = rep("cell1", 4L),
    v_call = c("IGHV1", "IGHV2", "IGLV1", "IGLV2"),
    j_call = c("IGHJ1", "IGHJ2", "IGLJ1", "IGLJ2"),
    junction_aa = c("CARW", "CBRW", "CASL", "CBSL"),
    locus = c("IGH", "IGH", "IGL", "IGL"),
    umi_count = umi_count
  )
}

make_paired_filter_test_idata <- function() {
  annotations <- tibble::tibble(
    imd_receptor_id = rep(1L, 4L),
    imd_barcode = c("bc1", "bc1", "bc2", "bc2"),
    imd_chain_id = seq_len(4L),
    imd_n_chains = rep(1L, 4L),
    locus = rep(c("IGH", "IGL"), 2L),
    cdr3_aa = rep(c("AAA", "CCC"), 2L),
    sample_id = rep(c("S1", "S2"), each = 2L)
  ) |>
    duckplyr::as_duckdb_tibble()

  ImmunData$new(
    schema = make_receptor_schema(
      features = "cdr3_aa",
      chains = c("IGH", "IGL")
    ),
    annotations = annotations
  )
}

make_basic_test_annotations <- function() {
  tibble::tibble(
    imd_receptor_id = seq_len(4L),
    imd_barcode = paste0("bc", seq_len(4L)),
    imd_chain_id = seq_len(4L),
    imd_n_chains = 1L,
    cdr3_aa = c("AAA", "AAT", "AAAA", "BBB"),
    v_call = c("V1", "V1", "V2", "V3"),
    sample_id = c("S1", "S1", "S2", "S2")
  ) |>
    duckplyr::as_duckdb_tibble()
}

make_single_chain_shared_receptor_test_data <- function() {
  tibble::tibble(
    cell_id = paste0("cell", seq_len(5L)),
    sample_id = c("Sample1", "Sample1", "Sample1", "Sample2", "Sample2"),
    v_call = c("IGHV1", "IGHV1", "IGHV2", "IGHV3", "IGHV4"),
    j_call = c("IGHJ1", "IGHJ1", "IGHJ2", "IGHJ3", "IGHJ4"),
    junction_aa = c("CARW", "CARW", "CBRW", "CCRW", "CDRW"),
    locus = "IGH",
    umi_count = c(100L, 150L, 200L, 250L, 300L)
  )
}

make_relaxed_pairing_test_data <- function() {
  tibble::tibble(
    cell_id = c(
      "normal_igl", "normal_igl",
      "normal_igk", "normal_igk",
      "artifact", "artifact", "artifact",
      "heavy_only",
      "light_only",
      "two_lights", "two_lights"
    ),
    v_call = c(
      "IGHV1", "IGLV1",
      "IGHV2", "IGKV2",
      "IGHV3", "IGLV3", "IGKV3",
      "IGHV4",
      "IGLV5",
      "IGLV6", "IGKV6"
    ),
    j_call = c(
      "IGHJ1", "IGLJ1",
      "IGHJ2", "IGKJ2",
      "IGHJ3", "IGLJ3", "IGKJ3",
      "IGHJ4",
      "IGLJ5",
      "IGLJ6", "IGKJ6"
    ),
    junction_aa = c(
      "CARW", "CASL",
      "CBRW", "CBSK",
      "CCRW", "CCSL", "CCSK",
      "CDRW",
      "CESL",
      "CFSL", "CFSK"
    ),
    locus = c(
      "IGH", "IGL",
      "IGH", "IGK",
      "IGH", "IGL", "IGK",
      "IGH",
      "IGL",
      "IGL", "IGK"
    ),
    umi_count = c(
      100L, 80L, 120L, 90L, 110L, 85L,
      75L, 130L, 70L, 60L, 65L
    )
  )
}
