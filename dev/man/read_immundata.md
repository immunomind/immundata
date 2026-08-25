

# Load an ImmunData object from disk

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_immundata_read.R#L112)

## Description

Continue an analysis later by reopening an ImmunData dataset saved on
disk. Use <code>read_immundata()</code> after restarting R, in another
script, or when another person gives you a dataset created by
<code>write_immundata()</code> or <code>read_repertoires()</code>. It is
that simple, just don’t forget to save the <code>ImmunData</code> object
first!

The unit restored retains all information: chain rows, cell and receptor
identifiers, repertoire and stratum definitions, and provenance. The
function does not change these biological units or the saved files. It
returns a new ImmunData object.

## Usage

<pre><code class='language-R'>read_immundata(
  path,
  tag = NULL,
  version = NULL,
  prudence = "stingy",
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
A character string. Path to a saved dataset directory. The directory
must contain <code>annotations.parquet</code> and
<code>metadata.json</code>. When <code>tag</code> is supplied, use the
project home directory that contains the <code>snapshots</code>
directory. Read more about snapshots on the website.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tag">tag</code>
</td>
<td>
A character string or <code>NULL</code>. Snapshot tag to read from
<code style="white-space: pre;">path/snapshots/\<tag\>/vNNN</code>. If
<code>NULL</code>, the default, <code>path</code> itself is read.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="version">version</code>
</td>
<td>
A non-negative integer or <code>NULL</code>. Snapshot version within
<code>tag</code>. For example, <code>1</code> reads <code>v001</code>.
If <code>NULL</code>, the default, the latest available version for the
tag is read. <code>version</code> can only be used with
<code>tag</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="prudence">prudence</code>
</td>
<td>
A character string. Memory protection used while reading the Parquet
data. This controls whether duckplyr may convert an intermediate result
from DuckDB-managed memory to an R data frame: <code>“stingy”</code>,
the default here, never permits conversion; <code>“thrifty”</code>
permits up to 1 million table cells (rows multiplied by columns); and
<code>“lavish”</code> permits conversion regardless of size. Here,
"table cells" does not mean biological cells. Passed to
<code>duckplyr::read_parquet_duckdb()</code>.
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

Read either a dataset directory directly or a versioned snapshot within
its project home.

## Value

A new, disk-backed ImmunData object representing the selected saved
state. Its provenance records the directory that was read.

## Choose the saved state

To reopen a dataset saved directly in a folder, supply that folder as
<code>path</code> and leave <code>tag</code> and <code>version</code> as
<code>NULL</code>.

To reopen a managed snapshot, supply the project home as
<code>path</code> and its tag. By default, the latest version for that
tag is read. Supply <code>version</code> when you need an exact earlier
state.

## Backend and serialized data

<code>annotations.parquet</code> stores the retained chain-level
annotation table. It is reopened as a lazy duckplyr table, so the
complete table does not need to be loaded into R memory.
<code>metadata.json</code> stores the format and package versions,
receptor, repertoire, and stratum schemas, the repertoire table, the
snapshot identifier, lineage events, and provenance paths.

Receptor and stratum views are reconstructed from this serialized state;
they are not stored as separate files. Please also mind, that the saved
files is an ImmunData-specific serialization, not an RDS file.

## See Also

<code>write_immundata()</code> for saving an analysis,
<code>read_repertoires()</code> for importing AIRR-seq files, ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Create a project home and save a filtered biological state as a snapshot
idata <- get_test_idata()
project_dir <- tempfile("immundata-project-")

project_idata <- write_immundata(
  idata,
  output_folder = project_dir,
  rehome = TRUE
)

fr_response <- project_idata |>
  filter(Response == "FR")

write_immundata(fr_response, tag = "fr-response")

# Read the exact first version of this snapshot
continued_fr <- read_immundata(
  project_dir,
  tag = "fr-response",
  version = 1
)

continued_fr |>
  collect() |>
  summarise(
    n_chains = n(),
    n_receptors = n_distinct(imd_receptor_id)
  )
```

    #> # A tibble: 1 × 2
    #>   n_chains n_receptors
    #> *    <int>       <dbl>
    #> 1      955         871

``` r
# Expected result: the snapshot contains the 955 chain rows and 871
# receptors from the FR response group.
#   n_chains n_receptors
#        955         871

list.files(file.path(project_dir, "snapshots", "fr-response"))
```

    #> [1] "v001"

``` r
# Expected result: "v001"

unlink(project_dir, recursive = TRUE)
```
