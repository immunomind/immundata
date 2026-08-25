

# Reduce repertoires to a common sampling depth

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_downsample.R#L122)

## Description

Use <code>downsample_immundata()</code> to reduce every repertoire to
the same number or fraction of observed cells or bulk sequence counts
before comparing repertoires. So, it is just a downsampling.

Use this function when different sequencing depths could affect a
comparison of repertoire diversity or composition. In single-cell data,
the sampling unit is a cell barcode and all selected chains from that
cell stay together. In bulk data with abundance values, the sampling
unit is one sequence count.

The function returns a new ImmunData object. The original object is not
changed.

## Usage

<pre><code class='language-R'>downsample_immundata(idata, n, seed = NULL)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An ImmunData object. For comparisons between repertoires, its
repertoires should already be defined with
<code>read_repertoires()</code> or <code>agg_repertoires()</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="n">n</code>
</td>
<td>
A number. Sampling depth. Use a value strictly between 0 and 1 for a
fraction, or a whole number greater than or equal to 1 for an absolute
number of cells or bulk sequence counts.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="seed">seed</code>
</td>
<td>
A non-negative integer or <code>NULL</code>. Used to reproduce the same
random sample. The default is <code>NULL</code>.
</td>
</tr>
</table>

## Value

A new ImmunData object containing the sampled chain observations. If the
input has repertoires or strata, their summaries are recalculated for
the sampled data.

## Meaning of <code>n</code> for single-cell data

<ul>
<li>

<code style="white-space: pre;">0 \< n \< 1</code> keeps
<code style="white-space: pre;">floor(n \* number of cells)</code> cells
from each repertoire.

</li>
<li>

<code>n \>= 1</code> keeps <code>n</code> cells from each repertoire.

</li>
</ul>

Cell barcodes are sampled without replacement. For paired receptors, all
retained chains belonging to a selected cell stay together.

## Meaning of <code>n</code> for bulk data

<ul>
<li>

<code style="white-space: pre;">0 \< n \< 1</code> keeps
<code style="white-space: pre;">floor(n \* total abundance)</code>
sequence counts from each repertoire.

</li>
<li>

<code>n \>= 1</code> keeps a total abundance of <code>n</code> from each
repertoire.

</li>
</ul>

Counts are sampled without replacement according to their observed
abundance. A retained receptor can therefore have a smaller abundance
than it had before downsampling. For example, <code>n = 1000</code>
makes the total retained abundance equal to 1000 in every repertoire
that originally contained at least 1000 counts.

If a requested whole-number <code>n</code> is larger than a repertoire,
that repertoire is returned unchanged and the function gives a warning.
If a fraction is so small that it selects zero units in any repertoire,
the function stops and asks for a larger value.

## Repertoire and strata summaries

When repertoires are defined, the function recalculates receptor counts,
proportions, repertoire sizes, and the number of repertoires containing
each receptor. Existing strata are also rebuilt, and their labels are
retained. When repertoires are not defined, the complete dataset is
treated as one sampling group and no repertoire summary is added.

## Backend and storage

Chain-level selection and reconstruction use the duckplyr annotation
table. The small table of sampling units is collected into R for random
sampling. The function does not overwrite the stored input object. Use
<code>write_immundata()</code> to save the returned object.

## See Also

<code>agg_repertoires()</code>, <code>filter_immundata()</code>,
<code>write_immundata()</code>

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Create two small bulk T-cell repertoires with different total abundances.
bulk_data <- tibble(
  Sample = c("Tumor", "Tumor", "Blood", "Blood"),
  cdr3_aa = c("CASSA", "CASSB", "CASSA", "CASSC"),
  v_call = c("TRBV1", "TRBV2", "TRBV1", "TRBV3"),
  abundance = c(20L, 5L, 4L, 6L)
)
bulk_file <- tempfile(fileext = ".tsv")
readr::write_tsv(bulk_data, bulk_file)

idata <- read_repertoires(
  path = bulk_file,
  schema = c("cdr3_aa", "v_call"),
  count_col = "abundance",
  repertoire_schema = "Sample",
  preprocess = NULL,
  postprocess = NULL,
  rename_columns = NULL,
  output_folder = tempfile("immundata-downsample-")
)

before <- idata$repertoires |>
  select(Sample, n_barcodes) |>
  rename(before = n_barcodes)

sampled <- downsample_immundata(idata, n = 5, seed = 42)

before |>
  left_join(
    sampled$repertoires |>
      select(Sample, n_barcodes) |>
      rename(after = n_barcodes),
    by = "Sample"
  ) |>
  arrange(Sample)
```

    #> # A tibble: 2 × 3
    #>   Sample before after
    #> * <chr>   <int> <dbl>
    #> 1 Blood      10     5
    #> 2 Tumor      25     5

``` r
# Expected result:
#   Sample before after
#   Blood      10     5
#   Tumor      25     5

# Each returned repertoire has five sequence counts. `idata` still has its
# original repertoire sizes of 10 and 25.
```
