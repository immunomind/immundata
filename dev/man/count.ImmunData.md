

# Count chain rows in ImmunData

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_count.R#L59)

## Description

Use <code>count()</code> to find how many chain rows are stored in an
ImmunData object.

Use this method for a quick check of dataset size. The unit counted is
one retained chain row. Each retained cell with a paired receptor
usually contributes two rows, one for each chain. The same receptor can
therefore contribute two rows for every cell carrying it. For bulk data
with an abundance column, this method counts table rows rather than the
summed sequence abundance.

The function returns a one-row duckplyr table. The original object is
not changed.

## Usage

<pre><code class='language-R'>## S3 method for class 'ImmunData'
count(x, ..., wt = NULL, sort = FALSE, name = NULL)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
An ImmunData object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
Additional arguments. Accepted for compatibility with
<code>dplyr::count()</code>, but currently ignored.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="wt">wt</code>
</td>
<td>
Any value or <code>NULL</code>. Accepted for compatibility with
<code>dplyr::count()</code>, but currently ignored.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sort">sort</code>
</td>
<td>
A logical value. Accepted for compatibility with
<code>dplyr::count()</code>, but currently ignored.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="name">name</code>
</td>
<td>
A character string or <code>NULL</code>. Accepted for compatibility with
<code>dplyr::count()</code>, but currently ignored. The result column is
always named <code>n</code>.
</td>
</tr>
</table>

## Details

This method currently provides only the total row count. The grouping,
weighting, sorting, and result-name arguments of
<code>dplyr::count()</code> are accepted for method compatibility but
are not applied.

The calculation runs on the duckplyr annotation table and can remain in
DuckDB. Use <code>dplyr::pull()</code> or <code>dplyr::collect()</code>
to bring the small result into R.

## Value

A one-row duckplyr table with an integer column named <code>n</code>.
This value is the number of rows in the chain-level annotation table.

## See Also

<code>dplyr::count()</code>, <code>dplyr::collect()</code>, ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)
idata <- get_test_idata()

idata |> count()
```

    #> # A duckplyr data frame: 1 variable
    #>       n
    #>   <int>
    #> 1  1902

``` r
# Expected result:
#      n
#   1902

# The result means that the object contains 1,902 retained chain rows.
# It does not mean that it contains 1,902 unique receptors.
```
