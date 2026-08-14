

# Read and process immune repertoire files to immundata

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_repertoires_read.R#L246)

## Description

This is the main function for reading immune repertoire data into the
<code>immundata</code> framework. It reads one or more repertoire files
(AIRR TSV, 10X CSV, Parquet), performs optional preprocessing and column
renaming, aggregates sequences into receptors based on a provided
schema, optionally joins manifest annotations, performs optional
postprocessing, and returns an <code>ImmunData</code> object.

The function handles different data types (bulk, single-cell) based on
the presence of <code>barcode_col</code> and <code>count_col</code>. For
efficiency with large datasets, it processes the data and saves
intermediate results (annotations) as a Parquet file before loading them
back into the final <code>ImmunData</code> object.

## Usage

<pre><code class='language-R'>read_repertoires(
  path,
  schema,
  manifest = NULL,
  barcode_col = NULL,
  count_col = NULL,
  locus_col = NULL,
  umi_col = NULL,
  preprocess = make_default_preprocessing(),
  postprocess = make_default_postprocessing(),
  rename_columns = imd_rename_cols("10x"),
  enforce_schema = TRUE,
  manifest_file_col = "file",
  output_folder = NULL,
  repertoire_schema = "&lt;auto&gt;",
  verbose = getOption("immundata.verbose", TRUE)
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="path">path</code>
</td>
<td>
Character vector. Path(s) to input repertoire files (e.g.,
<code>“/path/to/data/\*.tsv.gz”</code>). Supports glob patterns via
<code>Sys.glob()</code>. Files can be Parquet, CSV, TSV, or gzipped
versions thereof. All files must be of the same type. Alternatively,
pass the special string <code>“\<manifest\>”</code> to read file paths
from the <code>manifest</code> table (see <code>manifest</code> and
<code>manifest_file_col</code> params).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>

Defines how unique receptors are identified. Can be:

<ul>
<li>

A character vector of column names (e.g., <code>c(“v_call”, “j_call”,
“junction_aa”)</code>).

</li>
<li>

A schema object created by <code>make_receptor_schema()</code>, allowing
specification of chains for pairing (e.g.,
<code>make_receptor_schema(features = c(“v_call”, “junction_aa”), chains
= c(“TRA”, “TRB”))</code>).

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="manifest">manifest</code>
</td>
<td>
Optional. A data frame containing per-file annotations to be joined with
the repertoire data, read by <code>read_manifest()</code> function. If
<code>path = “\<manifest\>”</code>, this table <em>must</em> be provided
and contain the file paths column specified by
<code>manifest_file_col</code>. Default: <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="barcode_col">barcode_col</code>
</td>
<td>
Character(1). Name of the column containing cell barcodes or other
unique cell/clone identifiers for single-cell data. Triggers single-cell
processing logic in <code>agg_receptors()</code>. Default:
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="count_col">count_col</code>
</td>
<td>
Character(1). Name of the column containing UMI counts or frequency
counts for bulk sequencing data. Triggers bulk processing logic in
<code>agg_receptors()</code>. Default: <code>NULL</code>. Cannot be
specified if <code>barcode_col</code> is also specified.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="locus_col">locus_col</code>
</td>
<td>
Character(1). Name of the column specifying the receptor chain locus
(e.g., "TRA", "TRB", "IGH", "IGK", "IGL"). Required if
<code>schema</code> specifies chains for pairing. Default:
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="umi_col">umi_col</code>
</td>
<td>
Character(1). Name of the column containing UMI counts for single-cell
data. Required when <code>barcode_col</code> is used. It is used to
select the most abundant chain within a barcode (and within a locus for
paired-chain schemas). Default: <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="preprocess">preprocess</code>
</td>
<td>
List. A named list of functions to apply sequentially to the raw data
<em>before</em> receptor aggregation. Each function should accept a data
frame (or duckplyr_df) as its first argument. See
<code>make_default_preprocessing()</code> for examples. Default:
<code>make_default_preprocessing()</code>. Set to <code>NULL</code> or
<code>list()</code> to disable.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="postprocess">postprocess</code>
</td>
<td>
List. A named list of functions to apply sequentially to the annotation
data <em>after</em> receptor aggregation and manifest joining. Each
function should accept a data frame (or duckplyr_df) as its first
argument. See <code>make_default_postprocessing()</code> for examples.
Default: <code>make_default_postprocessing()</code>. Set to
<code>NULL</code> or <code>list()</code> to disable.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="rename_columns">rename_columns</code>
</td>
<td>
Named character vector. Optional mapping to rename columns in the input
files using <code>dplyr::rename()</code> syntax (e.g., <code>c(new_name
= “old_name”, barcode = “cell_id”)</code>). Renaming happens
<em>before</em> preprocessing and schema application. See
<code>imd_rename_cols()</code> for presets. Default:
<code>imd_rename_cols(“10x”)</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="enforce_schema">enforce_schema</code>
</td>
<td>
Logical(1). If <code>TRUE</code> (default), reading multiple files
requires them to have the exact same columns and types. If
<code>FALSE</code>, columns are unioned across files (potentially
slower, requires more memory). Default: <code>TRUE</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="manifest_file_col">manifest_file_col</code>
</td>
<td>
Character(1). The name of the column in the <code>manifest</code> table
that contains the full paths to the repertoire files. Only used when
<code>path = “\<manifest\>”</code>. Default: <code>“file”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="output_folder">output_folder</code>
</td>
<td>
Character(1). Path to a directory where intermediate processed
annotation data will be saved as <code>annotations.parquet</code> and
<code>metadata.json</code>. If <code>NULL</code> (default), a folder
named
<code style="white-space: pre;">immundata-\<basename_without_ext\></code>
is created in the same directory as the first input file specified in
<code>path</code>. The final <code>ImmunData</code> object reads from
these saved files. Default: <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="repertoire_schema">repertoire_schema</code>
</td>
<td>
Character vector, Function, <code>NULL</code>, or a special string.
Defines columns used to group annotations into distinct repertoires
(e.g., by sample or donor). <code>“\<manifest\>”</code> means group by
input file / manifest row. <code>“\<auto\>”</code> chooses
<code>“\<manifest\>”</code> behavior when <code>path =
“\<manifest\>”</code>, otherwise it groups by the internal input
filename column. If <code>NULL</code>, no repertoires are created.
Default: <code>“\<auto\>”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
Logical(1). Whether to print informative messages. Defaults to
<code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

The function executes the following steps:

<ol>
<li>

Validates inputs.

</li>
<li>

Determines the list of input files based on <code>path</code> and
<code>manifest</code>. Checks file extensions.

</li>
<li>

Reads data using <code>duckplyr</code> (<code>read_parquet_duckdb</code>
or <code>read_csv_duckdb</code>). Handles <code>.gz</code>.

</li>
<li>

Applies column renaming if <code>rename_columns</code> is provided.

</li>
<li>

Applies preprocessing steps sequentially if <code>preprocess</code> is
provided.

</li>
<li>

Aggregates sequences into receptors using <code>agg_receptors()</code>,
based on <code>schema</code>, <code>barcode_col</code>,
<code>count_col</code>, <code>locus_col</code>, and
<code>umi_col</code>. This creates the core annotation table.

</li>
<li>

Joins the <code>manifest</code> table if provided.

</li>
<li>

Applies postprocessing steps sequentially if <code>postprocess</code> is
provided.

</li>
<li>

Creates a temporary <code>ImmunData</code> object in memory.

</li>
<li>

Determines the <code>output_folder</code> path.

</li>
<li>

If <code>repertoire_schema</code> resolves to columns, calls
<code>agg_repertoires()</code> to define and summarize repertoires.

</li>
<li>

Saves the processed annotation table and metadata using
<code>write_immundata()</code> to the <code>output_folder</code>.

</li>
<li>

Loads the data back from the saved Parquet files using
<code>read_immundata()</code> to create the final <code>ImmunData</code>
object. This ensures the returned object is backed by efficient storage.

</li>
<li>

Returns the final <code>ImmunData</code> object.

</li>
</ol>

## Value

An <code>ImmunData</code> object containing the processed receptor
annotations. If <code>repertoire_schema</code> resolves to columns, the
object will also contain repertoire definitions and summaries calculated
by <code>agg_repertoires()</code>.

## See Also

ImmunData, <code>read_immundata()</code>,
<code>write_immundata()</code>, <code>read_manifest()</code>,
<code>agg_receptors()</code>, <code>agg_repertoires()</code>,
<code>make_receptor_schema()</code>,
<code>make_default_preprocessing()</code>,
<code>make_default_postprocessing()</code>

## Examples

``` r
library("immundata")

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
# Read multiple files using a manifest
# Create dummy files and a manifest
readr::write_tsv(airr_data[1:2, ], "sample1.tsv")
readr::write_tsv(airr_data[3:5, ], "sample2.tsv")
manifest <- data.frame(
  SampleID = c("S1", "S2"),
  Tissue = c("PBMC", "Tumor"),
  file = c(normalizePath("sample1.tsv"), normalizePath("sample2.tsv"))
)
readr::write_csv(manifest, "manifest.csv")

idata_multi <- read_repertoires(
  path = "<manifest>",
  manifest = manifest,
  schema = receptor_def,
  repertoire_schema = "SampleID", # Aggregate by SampleID
  output_folder = tempfile("immundata_multi_"),
  preprocess = make_default_preprocessing("airr"), # Use default AIRR filters
  postprocess = NULL
)

print(idata_multi)
print(idata_multi$repertoires) # Check repertoire summary

# Clean up dummy files
file.remove("my_sample.tsv", "sample1.tsv", "sample2.tsv", "manifest.csv")
unlink(out_dir, recursive = TRUE)
unlink(attr(idata_multi, "output_folder"), recursive = TRUE) # Get path used by function
```
