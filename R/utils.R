utils::globalVariables(c("Sample", "V.name"))

#' @importFrom checkmate checkClass
#' @importFrom checkmate assert
#' @importFrom checkmate assertChoice
#' @importFrom checkmate vname
assert_r6_gen <- function(
    x,
    classes = NULL,
    ordered = FALSE,
    cloneable = NULL,
    public = NULL,
    private = NULL,
    null.ok = FALSE,
    .var.name = vname(x),
    add = NULL) {
  if (missing(x)) {
    stop(sprintf(
      "argument \"%s\" is missing, with no default",
      .var.name
    ))
  }
  res <- check_class(x, "R6ClassGenerator", ordered, null.ok)
  makeAssertion(x, res, .var.name, add)
}

check_r6_gen <- function(x, classes = NULL, ordered = FALSE, cloneable = NULL,
                         public = NULL, private = NULL, null.ok = FALSE) {
  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("Install package 'R6' to perform checks of R6 classes")
  }
  if (is.null(x)) {
    if (null.ok) {
      return(TRUE)
    }
    return("Must be an R6 class, not 'NULL'")
  }
  if (!R6::is.R6Class(x)) {
    return(paste0(
      "Must be an R6 class", if (null.ok) " (or 'NULL')" else ""
    ))
  }
  checkClass(x, c(classes, "R6ClassGenerator"), ordered)
}


validate_clonotype_model <- function(.input) {
  # Define regex patterns for valid inputs
  valid_chain <- "^(a|b|g|d|k|l|h)+$"
  valid_sequence <- "cdr[123]|fr[1234]"
  valid_gene <- "^[vjdc]*$" # Allow missing V, J, D, C or any combination

  # Split the input string
  components <- unlist(stringr::str_split(.input, "\\+"))

  # Extract parts
  chain_component <- components[1]
  sequence_components <- components[grepl(valid_sequence, components)]
  gene_components <- components[grepl(valid_gene, components)]

  # Validate chain component
  if (!grepl(valid_chain, chain_component)) {
    stop("Invalid chain component: ", chain_component)
  }

  # Map chain shorthand to full names
  chain_names <- c(
    a = "Alpha", b = "Beta", g = "Gamma", d = "Delta",
    k = "Kappa", l = "Lambda", h = "Heavy"
  )
  chain <- strsplit(chain_component, "")[[1]]
  chain <- chain_names[chain]

  # Validate sequence components
  if (length(sequence_components) == 0) {
    stop("No valid sequence component found.")
  }

  # Sequence mapping is not changed (sequence components are explicit in input)

  # Validate gene components (now allowing for missing components)
  if (!all(gene_components %in% c("v", "j", "d", ""))) {
    stop("Invalid gene component(s) found.")
  }

  # Map gene shorthand to full names
  gene_names <- c(v = "V", j = "J", d = "D")
  gene <- gene_names[gene_components]

  # Construct and return the result
  result <- list(Chain = chain, Sequence = sequence_components, Gene = gene)
  return(result)
}


#' Run a Basic Test for ImmunDataLoader
#'
#' This function initializes an `ImmunDataLoader` and loads an example dataset.
#'
#' @param .backend Character string specifying the backend to use.
#'   Supported backends include `"df"` (data frame) and others defined in `ImmunDataLoader`.
#'
#' @return An `ImmunData` object containing the loaded dataset.
#' @export
immuntest <- function(.backend = "df") {
  loader <- ImmunDataLoader$new("./", .backend)

  imd <- loader$load("some file name doesn't matter")

  imd
}


#' Benchmark ImmunData Against dplyr Operations
#'
#' This function benchmarks operations on an `ImmunData` object and a
#' `dplyr`-based data frame, comparing performance in grouping and summarizing.
#'
#' @param .backend Character string specifying the backend to use.
#'   Supported backends include `"df"` (data frame) and others defined in `ImmunDataLoader`.
#'
#' @return Prints benchmarking results to the console.
#'
#' @importFrom dplyr group_by summarize n
#' @export
immunbench <- function(.backend = "df") {
  loader <- ImmunDataLoader$new("./", .backend)

  imd <- loader$load("some file name doesn't matter")

  bench_data <- imd$clone(deep = TRUE)$data()

  print(class(imd |> group_by(Sample, V.name)))
  print(class(bench_data))

  print("Imd -> dplyr")
  print(system.time({
    imd |>
      group_by(Sample, V.name) |>
      summarize(count = n())
  }))
  print(system.time({
    bench_data |>
      group_by(Sample, V.name) |>
      summarize(count = n())
  }))

  print("Dplyr -> imd")
  print(system.time({
    bench_data |>
      group_by(Sample, V.name) |>
      summarize(count = n())
  }))
  print(system.time({
    imd |>
      group_by(Sample, V.name) |>
      summarize(count = n())
  }))
}
