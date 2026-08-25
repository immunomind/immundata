

# Define biological repertoires and calculate receptor abundance

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_agg_repertoires.R#L136)

## Description

Use <code>agg_repertoires()</code> to define which receptor observations
belong to the same biological repertoire and calculate receptor
abundance within each repertoire.

Use this function after importing data without repertoire definitions,
or when you want to redefine repertoires using sample information. One
repertoire usually represents one biological sample. It can also
represent one sample and time-point combination. The columns in
<code>schema</code> define these groups.

The unit being defined is the repertoire. The function does not remove
chain rows or redefine cells or receptors. It returns a new ImmunData
object. The original object is not changed.

## Usage

<pre><code class='language-R'>agg_repertoires(
  idata,
  schema = "repertoire_id",
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
An ImmunData object containing receptor observations and the columns
named in <code>schema</code>. This is usually created by
<code>read_repertoires()</code> or <code>read_immundata()</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
A non-empty character vector. One or more column names that together
define a repertoire. For example, <code>“Sample”</code> creates one
repertoire per sample, and <code>c(“Sample”, “TimePoint”)</code> creates
one repertoire per sample and time-point combination. The default is
<code>“repertoire_id”</code>; this column must exist if the default is
used.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
A logical value. Accepted for consistency with other aggregation
functions. It currently does not change the output. Defaults to
<code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

The function calculates summaries at repertoire and receptor levels
while keeping the original chain rows.

## Value

A new ImmunData object with repertoire definitions and abundance
statistics. Its repertoire summary contains the <code>schema</code>
columns, <code>imd_repertoire_id</code>, <code>n_barcodes</code>, and
<code>n_receptors</code>. Its chain rows also contain
<code>imd_repertoire_id</code>, <code>imd_count</code>,
<code>imd_proportion</code>, and <code>n_repertoires</code>.

## What the function calculates

The returned repertoire summary contains one row for each repertoire:

<ul>
<li>

<code>imd_repertoire_id</code>: a new integer identifier for the
repertoire.

</li>
<li>

<code>n_barcodes</code>: the number of observed cells for single-cell
data, or the total abundance for bulk data.

</li>
<li>

<code>n_receptors</code>: the number of distinct receptors in the
repertoire.

</li>
</ul>

The function also adds these values to each chain row:

<ul>
<li>

<code>imd_repertoire_id</code>: the repertoire containing the row.

</li>
<li>

<code>imd_count</code>: the number of cells carrying that receptor in
single-cell data, or its summed abundance in bulk data, within the
repertoire.

</li>
<li>

<code>imd_proportion</code>: the receptor’s fraction of the repertoire,
calculated as <code>imd_count / n_barcodes</code>.

</li>
<li>

<code>n_repertoires</code>: the number of repertoires in which the
receptor occurs.

</li>
</ul>

Values calculated for a receptor are repeated on all chain rows
belonging to that receptor in the same repertoire.

Calling <code>agg_repertoires()</code> again replaces previous
repertoire definitions, receptor counts, proportions, and related strata
summaries.

## Backend and storage

Large-table calculations run on the duckplyr annotation table. The
annotation data remain lazy when the input is lazy. The small repertoire
summary is collected and stored in the returned object.

Aggregation can be expensive for a large dataset. After checking the
result, consider saving it so later analyses do not repeat the
calculation. Use <code>write_immundata(idata, tag = “by-sample”)</code>
to create a managed snapshot in the object’s project home. Managed
snapshots are versioned, so another write with the same tag creates a
new version and keeps the earlier version.

Use <code>write_immundata(idata, output_folder =
“path/to/result”)</code> when you need a standalone saved state in a
specific folder, for example to share it or to choose a new storage
location. Unlike a managed snapshot, writing to an existing explicit
folder replaces the ImmunData files in that folder. Both forms
materialize pending duckplyr calculations and return a disk-backed
object that can be reopened with <code>read_immundata()</code>.

## See Also

<code>read_repertoires()</code>, <code>agg_strata()</code>,
<code>write_immundata()</code>, ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Create a small bulk T-cell receptor dataset from two biological samples
bulk_data <- tibble(
  Sample = c("Tumor", "Tumor", "Blood", "Blood"),
  cdr3_aa = c("CASSA", "CASSB", "CASSA", "CASSC"),
  v_call = c("TRBV1", "TRBV2", "TRBV1", "TRBV3"),
  abundance = c(20L, 5L, 4L, 6L)
)

bulk_file <- tempfile(fileext = ".tsv")
readr::write_tsv(bulk_data, bulk_file)

# Import receptors without defining repertoires
idata <- read_repertoires(
  path = bulk_file,
  schema = c("cdr3_aa", "v_call"),
  count_col = "abundance",
  repertoire_schema = NULL,
  output_folder = tempfile("immundata-example-")
)

# Define one repertoire for each biological sample
sample_repertoires <- idata |>
  agg_repertoires(schema = "Sample")

sample_repertoires$repertoires |>
  select(Sample, n_barcodes, n_receptors) |>
  arrange(Sample)
```

    #> # A tibble: 2 × 3
    #>   Sample n_barcodes n_receptors
    #> * <chr>       <dbl>       <int>
    #> 1 Blood          10           2
    #> 2 Tumor          25           2

``` r
# Expected result:
#   Sample n_barcodes n_receptors
#   Blood          10           2
#   Tumor          25           2

# For example, CASSA forms 80% of the Tumor repertoire and 40% of the
# Blood repertoire. It occurs in two repertoires.

# For a large dataset, save the result as a managed snapshot so this
# aggregation does not need to run again.
saved_repertoires <- write_immundata(
  sample_repertoires,
  tag = "by-sample"
)
```
