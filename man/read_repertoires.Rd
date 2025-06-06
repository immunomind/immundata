% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_repertoires_read.R
\name{read_repertoires}
\alias{read_repertoires}
\title{Read and process immune repertoire files to immundata}
\usage{
read_repertoires(
  path,
  schema,
  metadata = NULL,
  barcode_col = NULL,
  count_col = NULL,
  locus_col = NULL,
  umi_col = NULL,
  preprocess = make_default_preprocessing(),
  postprocess = make_default_postprocessing(),
  rename_columns = imd_rename_cols("10x"),
  enforce_schema = TRUE,
  metadata_file_col = "File",
  output_folder = NULL,
  repertoire_schema = NULL
)
}
\arguments{
\item{path}{Character vector. Path(s) to input repertoire files (e.g.,
\code{"/path/to/data/*.tsv.gz"}). Supports glob patterns via \code{\link[=Sys.glob]{Sys.glob()}}.
Files can be Parquet, CSV, TSV, or gzipped versions thereof. All files
must be of the same type.
Alternatively, pass the special string \code{"<metadata>"} to read file paths
from the \code{metadata} table (see \code{metadata} and \code{metadata_file_col} params).}

\item{schema}{Defines how unique receptors are identified. Can be:
\itemize{
\item A character vector of column names (e.g., \code{c("v_call", "j_call", "junction_aa")}).
\item A schema object created by \code{\link[=make_receptor_schema]{make_receptor_schema()}}, allowing specification
of chains for pairing (e.g., \code{make_receptor_schema(features = c("v_call", "junction_aa"), chains = c("TRA", "TRB"))}).
}}

\item{metadata}{Optional. A data frame containing
metadata to be joined with the repertoire data, read by
\code{\link[=read_metadata]{read_metadata()}} function. If \code{path = "<metadata>"}, this table \emph{must}
be provided and contain the file paths column specified by \code{metadata_file_col}.
Default: \code{NULL}.}

\item{barcode_col}{Character(1). Name of the column containing cell barcodes
or other unique cell/clone identifiers for single-cell data. Triggers
single-cell processing logic in \code{\link[=agg_receptors]{agg_receptors()}}. Default: \code{NULL}.}

\item{count_col}{Character(1). Name of the column containing UMI counts or
frequency counts for bulk sequencing data. Triggers bulk processing logic
in \code{\link[=agg_receptors]{agg_receptors()}}. Default: \code{NULL}. Cannot be specified if \code{barcode_col} is also
specified.}

\item{locus_col}{Character(1). Name of the column specifying the receptor chain
locus (e.g., "TRA", "TRB", "IGH", "IGK", "IGL"). Required if \code{schema}
specifies chains for pairing. Default: \code{NULL}.}

\item{umi_col}{Character(1). Name of the column containing UMI counts for
single-cell data. Used during paired-chain processing to select the most
abundant chain per barcode per locus. Default: \code{NULL}.}

\item{preprocess}{List. A named list of functions to apply sequentially to the
raw data \emph{before} receptor aggregation. Each function should accept a
data frame (or duckplyr_df) as its first argument. See
\code{\link[=make_default_preprocessing]{make_default_preprocessing()}} for examples.
Default: \code{make_default_preprocessing()}. Set to \code{NULL} or \code{list()} to disable.}

\item{postprocess}{List. A named list of functions to apply sequentially to the
annotation data \emph{after} receptor aggregation and metadata joining. Each
function should accept a data frame (or duckplyr_df) as its first argument.
See \code{\link[=make_default_postprocessing]{make_default_postprocessing()}} for examples.
Default: \code{make_default_postprocessing()}. Set to \code{NULL} or \code{list()} to disable.}

\item{rename_columns}{Named character vector. Optional mapping to rename columns
in the input files using \code{dplyr::rename()} syntax (e.g.,
\code{c(new_name = "old_name", barcode = "cell_id")}). Renaming happens \emph{before}
preprocessing and schema application. See \code{\link[=imd_rename_cols]{imd_rename_cols()}} for presets.
Default: \code{imd_rename_cols("10x")}.}

\item{enforce_schema}{Logical(1). If \code{TRUE} (default), reading multiple files
requires them to have the exact same columns and types. If \code{FALSE}, columns
are unioned across files (potentially slower, requires more memory).
Default: \code{TRUE}.}

\item{metadata_file_col}{Character(1). The name of the column in the \code{metadata}
table that contains the full paths to the repertoire files. Only used when
\code{path = "<metadata>"}. Default: \code{"File"}.}

\item{output_folder}{Character(1). Path to a directory where intermediate
processed annotation data will be saved as \code{annotations.parquet} and
\code{metadata.json}. If \code{NULL} (default), a folder named
\verb{immundata-<basename_without_ext>} is created in the same directory as the
first input file specified in \code{path}. The final \code{ImmunData} object reads
from these saved files. Default: \code{NULL}.}

\item{repertoire_schema}{Character vector or Function. Defines columns used to
group annotations into distinct repertoires (e.g., by sample or donor).
If provided, \code{\link[=agg_repertoires]{agg_repertoires()}} is called after loading to add repertoire-level
summaries and metrics. Default: \code{NULL}.}
}
\value{
An \code{ImmunData} object containing the processed receptor annotations.
If \code{repertoire_schema} was provided, the object will also contain repertoire
definitions and summaries calculated by \code{\link[=agg_repertoires]{agg_repertoires()}}.
}
\description{
This is the main function for reading immune repertoire data into the
\code{immundata} framework. It reads one or more repertoire files (AIRR TSV,
10X CSV, Parquet), performs optional preprocessing and column renaming,
aggregates sequences into receptors based on a provided schema, optionally
joins external metadata, performs optional postprocessing, and returns
an \code{ImmunData} object.

The function handles different data types (bulk, single-cell) based on
the presence of \code{barcode_col} and \code{count_col}. For efficiency with large
datasets, it processes the data and saves intermediate results (annotations)
as a Parquet file before loading them back into the final \code{ImmunData} object.
}
\details{
The function executes the following steps:
\enumerate{
\item Validates inputs.
\item Determines the list of input files based on \code{path} and \code{metadata}. Checks file extensions.
\item Reads data using \code{duckplyr} (\code{read_parquet_duckdb} or \code{read_csv_duckdb}). Handles \code{.gz}.
\item Applies column renaming if \code{rename_columns} is provided.
\item Applies preprocessing steps sequentially if \code{preprocess} is provided.
\item Aggregates sequences into receptors using \code{\link[=agg_receptors]{agg_receptors()}}, based on \code{schema}, \code{barcode_col}, \code{count_col}, \code{locus_col}, and \code{umi_col}. This creates the core annotation table.
\item Joins the \code{metadata} table if provided.
\item Applies postprocessing steps sequentially if \code{postprocess} is provided.
\item Creates a temporary \code{ImmunData} object in memory.
\item Determines the \code{output_folder} path.
\item Saves the processed annotation table and metadata using \code{\link[=write_immundata]{write_immundata()}} to the \code{output_folder}.
\item Loads the data back from the saved Parquet files using \code{\link[=read_immundata]{read_immundata()}} to create the final \code{ImmunData} object. This ensures the returned object is backed by efficient storage.
\item If \code{repertoire_schema} is provided, calls \code{\link[=agg_repertoires]{agg_repertoires()}} on the loaded object to define and summarize repertoires.
\item Returns the final \code{ImmunData} object.
}
}
\examples{
\dontrun{
#
# Example 1: single-chain, one file
#
# Read a single AIRR TSV file, defining receptors by V/J/CDR3_aa
# Assume "my_sample.tsv" exists and follows AIRR format

# Create a dummy file for illustration
airr_data <- data.frame(
  sequence_id = paste0("seq", 1:5),
  v_call = c("TRBV1", "TRBV1", "TRBV2", "TRBV1", "TRBV3"),
  j_call = c("TRBJ1", "TRBJ1", "TRBJ2", "TRBJ1", "TRBJ1"),
  junction_aa = c("CASSL...", "CASSL...", "CASSD...", "CASSL...", "CASSF..."),
  productive = c(TRUE, TRUE, TRUE, FALSE, TRUE),
  locus = c("TRB", "TRB", "TRB", "TRB", "TRB")
)
readr::write_tsv(airr_data, "my_sample.tsv")

# Define receptor schema
receptor_def <- c("v_call", "j_call", "junction_aa")

# Specify output folder
out_dir <- tempfile("immundata_output_")

# Read the data (disabling default preprocessing for this simple example)
idata <- read_repertoires(
  path = "my_sample.tsv",
  schema = receptor_def,
  output_folder = out_dir,
  preprocess = NULL, # Disable default productive filter for demo
  postprocess = NULL # Disable default barcode prefixing
)

print(idata)
print(idata$annotations)

#
# Example 2: single-chain, multiple files
#
# Read multiple files using metadata
# Create dummy files and metadata
readr::write_tsv(airr_data[1:2, ], "sample1.tsv")
readr::write_tsv(airr_data[3:5, ], "sample2.tsv")
meta <- data.frame(
  SampleID = c("S1", "S2"),
  Tissue = c("PBMC", "Tumor"),
  FilePath = c(normalizePath("sample1.tsv"), normalizePath("sample2.tsv"))
)
readr::write_tsv(meta, "metadata.tsv")

idata_multi <- read_repertoires(
  path = "<metadata>",
  metadata = meta,
  metadata_file_col = "FilePath",
  schema = receptor_def,
  repertoire_schema = "SampleID", # Aggregate by SampleID
  output_folder = tempfile("immundata_multi_"),
  preprocess = make_default_preprocessing("airr"), # Use default AIRR filters
  postprocess = NULL
)

print(idata_multi)
print(idata_multi$repertoires) # Check repertoire summary

# Clean up dummy files
file.remove("my_sample.tsv", "sample1.tsv", "sample2.tsv", "metadata.tsv")
unlink(out_dir, recursive = TRUE)
unlink(attr(idata_multi, "output_folder"), recursive = TRUE) # Get path used by function
}
}
\seealso{
\link{ImmunData}, \code{\link[=read_immundata]{read_immundata()}}, \code{\link[=write_immundata]{write_immundata()}}, \code{\link[=read_metadata]{read_metadata()}},
\code{\link[=agg_receptors]{agg_receptors()}}, \code{\link[=agg_repertoires]{agg_repertoires()}}, \code{\link[=make_receptor_schema]{make_receptor_schema()}},
\code{\link[=make_default_preprocessing]{make_default_preprocessing()}}, \code{\link[=make_default_postprocessing]{make_default_postprocessing()}}
}
