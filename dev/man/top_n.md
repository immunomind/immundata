

# Keep the most abundant receptors in ImmunData

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_top_n.R#L70)

## Description

Use <code>top_n()</code> to select abundant receptors for further
analysis. For example, keep the three most abundant receptors in each
sample to compare their sequences or cell annotations.

If repertoires are defined, selection is performed separately in each
repertoire. Otherwise, the whole dataset is treated as one group. The
function returns a new ImmunData object. The original object is not
changed.

## Usage

<pre><code class='language-R'>top_n(idata, n = 1L, order_by = "imd_count", with_ties = FALSE)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An ImmunData object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="n">n</code>
</td>
<td>
A non-negative whole number. How many receptors to keep in each
repertoire, or in the whole dataset when repertoires are not defined.
The default is <code>1L</code>. Set <code>n = 0</code> for an empty
result. If fewer receptors are available, all available receptors are
kept.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="order_by">order_by</code>
</td>
<td>
A character string. Use <code>“imd_count”</code>, the default, to select
by the number of cells or bulk sequence counts. Use
<code>“imd_proportion”</code> to select by each receptor’s fraction of
its repertoire, or of the whole dataset when repertoires are not
defined.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="with_ties">with_ties</code>
</td>
<td>
A logical value. If <code>FALSE</code>, the default, keep at most
<code>n</code> receptors per group. If <code>TRUE</code>, keep all
receptors tied at the selection limit, which can return more than
<code>n</code> receptors.
</td>
</tr>
</table>

## Details

All observations belonging to a selected receptor in a selected
repertoire are kept, including both chains of a paired receptor. A
receptor selected in one repertoire is kept in another repertoire only
if it is also selected there.

Without repertoire definitions, counts are calculated across the whole
input for selection. Each cell contributes once, and bulk observations
contribute their abundance values. No repertoire definitions are added
to the result.

When repertoires are defined, selection uses the input counts or
proportions. Repertoire and strata summaries are then recalculated for
the selected data, as in <code>filter_immundata()</code>. Returned
proportions describe each receptor’s share of its filtered repertoire
and can be larger than the input values.

With <code>with_ties = FALSE</code>, the default, at most <code>n</code>
receptors are kept per repertoire, or in total when repertoires are not
defined. Receptor identifiers determine which equally abundant receptors
are kept. Set <code>with_ties = TRUE</code> to include all receptors
with the same abundance at the selection limit.

## Value

A new ImmunData object containing all observations of the selected
receptors. Existing repertoire and strata summaries are recalculated for
the selected data.

## See Also

<code>filter_immundata()</code>, <code>agg_repertoires()</code>,
ImmunData

## Examples

``` r
library("immundata")

library(immundata)

options(immundata.verbose = FALSE)
idata <- get_test_idata()

# Keep up to three receptors with the highest counts in each repertoire
top_receptors <- top_n(idata, n = 3L)

top_receptors$repertoires |>
  select(imd_repertoire_id, n_receptors)
```

    #> # A tibble: 2 × 2
    #>   imd_repertoire_id n_receptors
    #> *             <int>       <int>
    #> 1                 1           3
    #> 2                 2           3

``` r
# Include all receptors tied with the third receptor in each repertoire
top_with_ties <- top_n(idata, n = 3L, with_ties = TRUE)
```
