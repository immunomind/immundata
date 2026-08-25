

# Save an ImmunData object to disk

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_immundata_write.R#L106)

## Description

Save <code>ImmunData</code> to disk so you can close R and continue the
work later (I cannot believe it, but it works, I tried it). Use
<code>write_immundata()</code> after importing or transforming
repertoire data, or when you want a named snapshot before the next
analysis step.

The unit saved is the complete ImmunData object. This includes retained
chain rows, cell and receptor identifiers, repertoire and stratum
definitions, and provenance. Saving does not add, remove, or change any
biological unit.

## Usage

<pre><code class='language-R'>write_immundata(
  idata,
  output_folder = NULL,
  tag = NULL,
  rehome = FALSE,
  compression = "zstd",
  compression_level = 9,
  verbose = getOption("immundata.verbose", TRUE)
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An ImmunData object you want to save.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="output_folder">output_folder</code>
</td>
<td>
A character string or <code>NULL</code>. Directory in which to write
<code>annotations.parquet</code> and <code>metadata.json</code>. If
<code>NULL</code>, the default, a managed snapshot is created at
<code style="white-space: pre;">home_path/snapshots/\<tag\>/vNNN</code>.
The home path comes from the object’s provenance.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tag">tag</code>
</td>
<td>
A character string or <code>NULL</code>. Snapshot tag. With
<code>output_folder = NULL</code>, it names the managed snapshot series;
if <code>tag</code> is also <code>NULL</code>, <code>“default”</code> is
used. With an explicit <code>output_folder</code>, a supplied tag is
recorded in the lineage but does not change the output path.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="rehome">rehome</code>
</td>
<td>
A logical value. Whether an explicit <code>output_folder</code> becomes
the home for future managed snapshots. The default is
<code>FALSE</code>, which preserves an existing home. If the object has
no home yet, its first explicit output folder becomes the home with
either value. <code>TRUE</code> requires an explicit
<code>output_folder</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="compression">compression</code>
</td>
<td>
A character string or <code>NULL</code>. Parquet compression codec
passed to DuckDB. The default is <code>“zstd”</code>. Use
<code>NULL</code> to let DuckDB choose.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="compression_level">compression_level</code>
</td>
<td>
A number or <code>NULL</code>. Compression level for codecs that support
it. The default is <code>9</code>. Use <code>NULL</code> to let DuckDB
choose.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
A logical value. Whether to print progress and summary messages.
Defaults to <code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

Save to an explicit folder for a direct saved state, or use the object’s
home to create a versioned managed snapshot.

## Value

Invisibly returns a newly reopened, disk-backed ImmunData object with
provenance for the new save. The input <code>idata</code> remains
unchanged.

## Choose how to save

Supply <code>output_folder</code> to save a standalone state in a
specific directory. This is useful when sharing a dataset or choosing
its first project home. If the directory already contains an ImmunData
dataset, its <code>annotations.parquet</code> and
<code>metadata.json</code> are replaced.

Leave <code>output_folder = NULL</code> to create a managed snapshot.
The function uses the object’s home path and writes the next version
under <code style="white-space: pre;">snapshots/\<tag\>/vNNN</code>, for
example <code>snapshots/baseline/v001</code>. Later writes with the same
tag create <code>v002</code>, <code>v003</code>, and so on; earlier
versions remain available. Use <code>read_immundata()</code> with
<code>tag</code> and <code>version</code> to reopen one.

Every save receives a new snapshot identifier and appends a provenance
event. The returned object records the new saved directory as its
current path.

## Backend and serialization

The retained chain-level annotation table is materialized as compressed
<code>annotations.parquet</code>. Materialization executes any pending
lazy duckplyr calculations. <code>metadata.json</code> serializes format
and package versions, receptor, repertoire, and stratum schemas, the
small repertoire table, the snapshot identifier, lineage events, and
provenance paths.

Receptor and stratum views are not written as separate files; they can
be reconstructed from the annotation table and metadata. This Parquet
and JSON pair is an ImmunData-specific serialization, not an RDS file.

## See Also

<code>read_immundata()</code> for continuing a saved analysis,
<code>read_repertoires()</code> for importing AIRR-seq files, ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Save a small immune-repertoire analysis
idata <- get_test_idata()
save_dir <- tempfile("saved-immundata-")

saved_idata <- write_immundata(idata, save_dir)

list.files(save_dir)
```

    #> [1] "annotations.parquet" "metadata.json"

``` r
# Expected result: the analysis is serialized as two files.
# [1] "annotations.parquet" "metadata.json"

# Continue the analysis from the saved files
continued_idata <- read_immundata(save_dir)

continued_idata |>
  collect() |>
  summarise(
    n_chains = n(),
    n_receptors = n_distinct(imd_receptor_id)
  )
```

    #> # A tibble: 1 × 2
    #>   n_chains n_receptors
    #> *    <int>       <dbl>
    #> 1     1902        1668

``` r
# Expected result: all 1,902 chain rows and 1,668 receptors are restored.
#   n_chains n_receptors
#       1902        1668

unlink(save_dir, recursive = TRUE)
```
