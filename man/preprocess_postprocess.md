

# Process chain rows while reading repertoire files

## Description

Use these functions to preprocess or postprocess rows of the input data
before returning the final <code>ImmunData</code> object to the session.
A couple of example use cases: keep productive receptor chains, remove
technical columns, or make cell barcodes unique while importing
repertoire files with <code>read_repertoires()</code>.

The defaults provide steps for common AIRR or 10x inputs. Use an
individual step when your files need only one operation or when you are
building a custom <code>preprocess</code> or <code>postprocess</code>
list.

Preprocessing changes chain rows before receptors are defined. Barcode
prefixing changes the cell identifier after receptor and manifest
information are combined. The input files and input table are not
changed: every step returns a new duckplyr table.

## Usage

<pre><code class='language-R'>make_default_preprocessing(format = c("default", "airr", "10x"))

make_default_postprocessing()

make_exclude_columns(cols = imd_drop_cols("airr"))

make_productive_filter(col_name = c("productive"), truthy = TRUE)

make_barcode_prefix(prefix_col = "Prefix")
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="format">format</code>
</td>
<td>
A character string. One input format: <code>“default”</code>,
<code>“airr”</code>, or <code>“10x”</code>. The default is
<code>“default”</code>. This choice controls which technical columns are
removed. It does not rename columns.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="cols">cols</code>
</td>
<td>
A character vector. Columns to remove. The default is
<code>imd_drop_cols(“airr”)</code>. Use <code>character()</code> to
create a step that removes no columns.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="col_name">col_name</code>
</td>
<td>
A character string. Column containing the productive-chain indicator.
The default is <code>“productive”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="truthy">truthy</code>
</td>
<td>
A vector. Values that mean the chain is productive. Values are compared
as text. The default is <code>TRUE</code>; use a character vector when
the source uses several representations, for example <code>c(“TRUE”,
“true”, “1”)</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="prefix_col">prefix_col</code>
</td>
<td>
A character vector. One or more candidate columns containing the text to
place before each cell barcode. The first candidate present in the data
is used. The default is <code>“Prefix”</code>.
</td>
</tr>
</table>

## Value

<code>make_default_preprocessing()</code> and
<code>make_default_postprocessing()</code> return named lists of
processing functions. The other functions return one processing
function. Each processing function accepts a duckplyr table as its first
argument, accepts unused arguments through <code>…</code>, and returns a
new duckplyr table.

## Choose processing steps

<ul>
<li>

<code>make_default_preprocessing()</code> returns two steps. The first
removes common technical columns. The second keeps rows whose
<code>productive</code> value indicates a productive chain. If the
<code>productive</code> column is absent, the filtering step gives a
warning and keeps all rows.

</li>
<li>

<code>make_default_postprocessing()</code> returns one step that adds a
sample-specific prefix to cell barcodes. If the prefix column is absent,
the step gives a warning and leaves barcodes unchanged.

</li>
<li>

<code>make_exclude_columns()</code> creates one step that removes the
columns in <code>cols</code>. Column names that are not present are
ignored.

</li>
<li>

<code>make_productive_filter()</code> creates one step that keeps rows
whose value in <code>col_name</code> matches any value in
<code>truthy</code>.

</li>
<li>

<code>make_barcode_prefix()</code> creates one step that joins a prefix,
such as <code>“Tumor\_”</code>, to the start of each
<code>imd_barcode</code> value.

</li>
</ul>

<code>read_repertoires()</code> applies functions in list order. You can
therefore add, remove, or reorder steps in a custom list.

## Input formats

For <code>make_default_preprocessing()</code>, <code>format =
“default”</code> removes the union of the standard AIRR and 10x
technical columns. Use <code>format = “airr”</code> or <code>format =
“10x”</code> to remove only the columns expected for that format. All
three defaults recognize common text representations of a productive
value, including <code>“TRUE”</code>, <code>“true”</code>,
<code>“yes”</code>, and <code>“1”</code>.

## See Also

<code>read_repertoires()</code>, <code>imd_drop_cols()</code>,
<code>imd_rename_cols()</code>

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

# Three 10x chain rows from two samples. One chain is non-productive.
chains <- duckplyr::duckdb_tibble(
  imd_barcode = c("AAAC-1", "AAAG-1", "AATT-1"),
  cdr3_aa = c("CASSA", "CASSB", "CASSC"),
  productive = c("TRUE", "FALSE", "TRUE"),
  full_length = c(TRUE, TRUE, TRUE),
  Prefix = c("Tumor_", "Tumor_", "Blood_")
)

# read_repertoires() performs these calls for you. They are shown here to
# make the effect of each list clear.
prepared <- Reduce(
  function(data, step) step(data),
  make_default_preprocessing("10x"),
  init = chains
)
prepared <- Reduce(
  function(data, step) step(data),
  make_default_postprocessing(),
  init = prepared
)

prepared |>
  collect() |>
  select(imd_barcode, cdr3_aa, productive)
```

    #> # A tibble: 2 × 3
    #>   imd_barcode  cdr3_aa productive
    #> * <chr>        <chr>   <chr>     
    #> 1 Tumor_AAAC-1 CASSA   TRUE      
    #> 2 Blood_AATT-1 CASSC   TRUE

``` r
# Expected result:
#   imd_barcode  cdr3_aa productive
#   Tumor_AAAC-1 CASSA   TRUE
#   Blood_AATT-1 CASSC   TRUE

# The non-productive chain was removed, `full_length` was dropped, and the
# sample prefixes made the retained cell barcodes unique.
```
