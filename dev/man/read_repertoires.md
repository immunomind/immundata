

# Read immune repertoire files into ImmunData

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_repertoires_read.R#L260)

## Description

<code>read_repertoires()</code> is the main function for importing
AIRR-seq data. It reads one or more repertoire files, defines biological
receptors, adds sample information from an optional manifest, and
returns an ImmunData object.

The function saves the processed data in <code>output_folder</code>.
This lets you work with large datasets without loading everything into
memory and reopen the result later with <code>read_immundata()</code>.

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

One or more repertoire file paths, or a glob pattern such as
<code>“/path/to/data/\*.tsv.gz”</code>. Supported formats are Parquet,
CSV, TSV, and gzipped CSV or TSV. All input files must have the same
file type.

Use <code>“\<manifest\>”</code> to take file paths from
<code>manifest</code> instead. In that case, <code>manifest</code> is
required.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>

Definition of receptor identity. Supply either:

<ul>
<li>

A character vector naming the features that must match, such as
<code>c(“v_call”, “j_call”, “junction_aa”)</code>.

</li>
<li>

An object created by <code>make_receptor_schema()</code> to select one
locus or pair two loci from the same cell.

</li>
</ul>
Use column names as they appear <em>after</em>
<code>rename_columns</code> is applied. For example, if the input
columns are <code>CDR3.aa</code> and <code>V.name</code>, use
<code>rename_columns = c(cdr3_aa = “CDR3.aa”, v_call = “V.name”)</code>
together with <code>schema = c(“cdr3_aa”, “v_call”)</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="manifest">manifest</code>
</td>
<td>
An optional data frame with one row per repertoire file and columns
containing sample, donor, tissue, treatment, or other information. Use
<code>read_manifest()</code> to read and validate a manifest file.
Manifest paths must be unique. When <code>path = “\<manifest\>”</code>,
the column named by <code>manifest_file_col</code> supplies the
repertoire file paths. The default is <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="barcode_col">barcode_col</code>
</td>
<td>
Name of the column containing cell barcodes. Supplying it selects
single-cell processing, requires <code>umi_col</code>, and prevents use
of <code>count_col</code>. Use the column name after renaming. The
default is <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="count_col">count_col</code>
</td>
<td>
Name of the column containing non-negative abundance values for bulk
repertoire data. It cannot be used with <code>barcode_col</code>. Use
the column name after renaming. The default is <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="locus_col">locus_col</code>
</td>
<td>
Name of the column containing receptor loci such as <code>“TRA”</code>,
<code>“TRB”</code>, <code>“IGH”</code>, <code>“IGK”</code>, or
<code>“IGL”</code>. It is required when <code>schema</code> selects or
pairs chains. Use the column name after renaming. The default is
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="umi_col">umi_col</code>
</td>
<td>
Name of the column containing per-chain UMI or read counts. It is
required whenever <code>barcode_col</code> is supplied and is used to
choose one chain when a cell contains several chains from the same
locus. Use the column name after renaming. The default is
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="preprocess">preprocess</code>
</td>
<td>
A named list of functions applied in order before receptors are defined.
Each function must accept a duckplyr table as its first argument and
return a duckplyr table. By default,
<code>make_default_preprocessing()</code> removes selected technical
columns and keeps productive sequences when a <code>productive</code>
column is available. Use <code>NULL</code> or <code>list()</code> to
disable preprocessing.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="postprocess">postprocess</code>
</td>
<td>
A named list of functions applied in order after receptors are defined
and manifest information is added. Each function must accept and return
a duckplyr table. By default, <code>make_default_postprocessing()</code>
prefixes cell barcodes when the manifest contains a <code>Prefix</code>
column. Use <code>NULL</code> or <code>list()</code> to disable
postprocessing.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="rename_columns">rename_columns</code>
</td>
<td>
An optional named character vector in the form <code>c(new_name =
“old_name”)</code>. Renaming occurs before preprocessing and receptor
definition. The default, <code>imd_rename_cols(“10x”)</code>,
standardizes common 10x names such as <code>v_gene</code> to
<code>v_call</code> and <code>chain</code> to <code>locus</code> when
those source columns are present. Use <code>NULL</code> to preserve all
input names.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="enforce_schema">enforce_schema</code>
</td>
<td>
Whether multiple input files must have the same columns and column
types. The default is <code>TRUE</code>. If <code>FALSE</code>, columns
are combined by name and missing values are added where necessary. This
is slower and can require more memory.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="manifest_file_col">manifest_file_col</code>
</td>
<td>
Name of the manifest column containing repertoire file paths when
<code>path = “\<manifest\>”</code>. The default is <code>“file”</code>.
Use the same name passed as <code>file_col</code> to
<code>read_manifest()</code> when it is not <code>“file”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="output_folder">output_folder</code>
</td>
<td>
Directory in which to write <code>annotations.parquet</code> and
<code>metadata.json</code>. These files are the persistent backing
storage for the returned object. If <code>NULL</code>, a folder
beginning with <code style="white-space: pre;">immundata-</code> is
created beside the first input file. Supplying an existing folder
replaces its <code>annotations.parquet</code> and
<code>metadata.json</code>. The default is <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="repertoire_schema">repertoire_schema</code>
</td>
<td>

Definition of repertoires. Supply one of:

<ul>
<li>

A character vector naming columns that define one repertoire, such as
<code>c(“donor”, “timepoint”)</code>.

</li>
<li>

<code>“\<auto\>”</code>, the default. This creates one repertoire per
input file, or one per manifest row when <code>path =
“\<manifest\>”</code>.

</li>
<li>

<code>“\<manifest\>”</code>, which uses all manifest columns when a
manifest is available, or the input filename otherwise.

</li>
<li>

<code>NULL</code> to leave repertoires undefined.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
Whether to print progress and summary messages. Defaults to
<code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

The required arguments depend on how receptor observations are
represented in the input files.

## Value

A disk-backed ImmunData object containing the retained chain rows,
receptor definitions, manifest annotations, and ingestion provenance. If
<code>repertoire_schema</code> is not <code>NULL</code>, it also
contains repertoire definitions and summary statistics calculated by
<code>agg_repertoires()</code>.

## Choose arguments for your data

<ul>
<li>

<strong>Uncounted repertoire table:</strong> Supply <code>schema</code>.
Leave <code>barcode_col</code> and <code>count_col</code> as
<code>NULL</code>. Each retained row represents one observed chain.

</li>
<li>

<strong>Bulk repertoire with abundance:</strong> Supply
<code>schema</code> and <code>count_col</code>. The abundance values are
preserved for later repertoire statistics.

</li>
<li>

<strong>Single-cell, one selected chain:</strong> Use
<code>make_receptor_schema()</code> with one chain and supply
<code>barcode_col</code>, <code>locus_col</code>, and
<code>umi_col</code>.

</li>
<li>

<strong>Single-cell, paired chains:</strong> Use
<code>make_receptor_schema()</code> with two chains and supply
<code>barcode_col</code>, <code>locus_col</code>, and
<code>umi_col</code>. Only cells containing both requested chains are
retained.

</li>
<li>

<strong>Single-cell, relaxed paired chains:</strong> Use a schema such
as <code>chains = c(“IGH”, “IGL|IGK”)</code> with
<code>barcode_col</code>, <code>locus_col</code>, and
<code>umi_col</code>. This accepts either an IGH-IGL or IGH-IGK
receptor.

</li>
</ul>

In single-cell data, the chain with the highest <code>umi_col</code>
value is retained when a cell contains several chains from the same
locus.

## What happens by default

Unless you override the relevant arguments,
<code>read_repertoires()</code>:

<ul>
<li>

standardizes common 10x column names;

</li>
<li>

removes selected technical columns;

</li>
<li>

keeps productive sequences when productivity information is present;

</li>
<li>

prefixes barcodes when a manifest <code>Prefix</code> column is present;

</li>
<li>

creates repertoires automatically; and

</li>
<li>

writes the completed dataset to disk.

</li>
</ul>

Set <code>rename_columns</code>, <code>preprocess</code>,
<code>postprocess</code>, or <code>repertoire_schema</code> to
<code>NULL</code> to disable the corresponding behavior.

## Processing order

The function:

<ol>
<li>

finds and reads the input files as one duckplyr table;

</li>
<li>

renames columns;

</li>
<li>

applies preprocessing;

</li>
<li>

defines receptors using <code>schema</code>;

</li>
<li>

adds manifest information;

</li>
<li>

applies postprocessing;

</li>
<li>

defines repertoires when requested; and

</li>
<li>

writes and reopens the completed ImmunData dataset.

</li>
</ol>

## Manifests and repertoires

A manifest <em>annotates</em> each input file with biological
information. The <code>repertoire_schema</code> argument chooses which
annotation columns <em>define a repertoire</em> and therefore determine
receptor counts and proportions.

With <code>path = “\<manifest\>”</code> and the default
<code>repertoire_schema = “\<auto\>”</code>, all manifest columns are
used and each manifest row becomes one repertoire. With an explicit file
path or vector of paths, <code>“\<auto\>”</code> creates one repertoire
per input file.

## Output storage

The output folder is not a temporary cache. The returned object reads
its receptor annotations from <code>annotations.parquet</code>, while
<code>metadata.json</code> stores its schemas, repertoire summaries, and
provenance. Keep this folder for as long as you need the object, or
reopen it later with <code>read_immundata()</code>.

<strong>Important:</strong> Reusing the same <code>output_folder</code>
replaces the existing <code>annotations.parquet</code> and
<code>metadata.json</code> without creating a new version.

## See Also

<code>read_manifest()</code>, <code>make_receptor_schema()</code>,
<code>agg_receptors()</code>, <code>agg_repertoires()</code>,
<code>make_default_preprocessing()</code>,
<code>make_default_postprocessing()</code>,
<code>read_immundata()</code>, <code>write_immundata()</code>, ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Read one bulk AIRR file and preserve its abundance column
bulk_file <- system.file(
  "extdata/tsv",
  "sample_0_1k.tsv",
  package = "immundata"
)

bulk_idata <- read_repertoires(
  path = bulk_file,
  schema = c("cdr3_aa", "v_call"),
  count_col = "counts",
  output_folder = tempfile("immundata-bulk-")
)

tibble(
  n_records = bulk_idata |> count() |> pull(n),
  n_receptors = bulk_idata$receptors |> count() |> collect() |> pull(n),
  n_repertoires = nrow(bulk_idata$repertoires)
)
```

    #> # A tibble: 1 × 3
    #>   n_records n_receptors n_repertoires
    #>       <int>       <int>         <int>
    #> 1       955         871             1

``` r
# Expected result:
#   n_records n_receptors n_repertoires
#         955         871             1

# Read multiple files and their sample information from a manifest
manifest_path <- system.file(
  "extdata/tsv",
  "manifest.csv",
  package = "immundata"
)
manifest <- read_manifest(manifest_path)

manifest_idata <- read_repertoires(
  path = "<manifest>",
  manifest = manifest,
  schema = c("cdr3_aa", "v_call"),
  count_col = "counts",
  output_folder = tempfile("immundata-manifest-")
)

manifest_idata$repertoires |>
  select(Therapy, Response, n_barcodes, n_receptors) |>
  arrange(Response)
```

    #> # A tibble: 2 × 4
    #>   Therapy Response n_barcodes n_receptors
    #> * <chr>   <chr>         <int>       <int>
    #> 1 ICI     FR             4725         871
    #> 2 CAR-T   PR             4758         867

``` r
# Expected result:
#   Therapy Response n_barcodes n_receptors
#   ICI     FR             4725         871
#   CAR-T   PR             4758         867

# Read paired TRA-TRB receptors from a small single-cell table
paired_input <- tibble(
  cell_id = c("cell1", "cell1", "cell2", "cell2", "cell3"),
  locus = c("TRA", "TRB", "TRA", "TRB", "TRA"),
  v_call = c("TRAV1", "TRBV1", "TRAV1", "TRBV1", "TRAV2"),
  j_call = c("TRAJ1", "TRBJ1", "TRAJ1", "TRBJ1", "TRAJ2"),
  junction_aa = c("CAVA", "CASSB", "CAVA", "CASSB", "CAVC"),
  umi_count = c(10L, 8L, 12L, 9L, 7L)
)
paired_file <- tempfile(fileext = ".tsv")
readr::write_tsv(paired_input, paired_file)

paired_idata <- read_repertoires(
  path = paired_file,
  schema = make_receptor_schema(
    features = c("v_call", "j_call", "junction_aa"),
    chains = c("TRA", "TRB")
  ),
  barcode_col = "cell_id",
  locus_col = "locus",
  umi_col = "umi_count",
  repertoire_schema = NULL,
  output_folder = tempfile("immundata-paired-")
)

tibble(
  n_chains = paired_idata |> count() |> pull(n),
  n_cells = paired_idata |> collect() |> distinct(imd_barcode) |> nrow(),
  n_receptors = paired_idata$receptors |> count() |> collect() |> pull(n)
)
```

    #> # A tibble: 1 × 3
    #>   n_chains n_cells n_receptors
    #>      <int>   <int>       <int>
    #> 1        4       2           1

``` r
# Expected result:
#   n_chains n_cells n_receptors
#          4       2           1
```
