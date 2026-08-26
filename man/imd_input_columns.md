

# Get input-column presets

## Description

Use these helpers to inspect or customize the column renaming and
removal presets used by <code>read_repertoires()</code>.

<code>imd_rename_cols()</code> returns mappings from standard output
names to source names. <code>imd_drop_cols()</code> returns technical
columns that can usually be removed before receptors are defined. These
functions return definitions only; they do not change input files or an
ImmunData object.

## Usage

<pre><code class='language-R'>imd_rename_cols(format = "default")

imd_drop_cols(format = "airr")
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="format">format</code>
</td>
<td>
A character string. The input format preset. For
<code>imd_rename_cols()</code>, use <code>“default”</code> or
<code>“10x”</code>; the default is <code>“default”</code>. For
<code>imd_drop_cols()</code>, use <code>“universal”</code>,
<code>“airr”</code>, or <code>“10x”</code>; the default is
<code>“airr”</code>.
</td>
</tr>
</table>

## Value

<code>imd_rename_cols()</code> returns a named character vector in the
form <code>c(new_name = “source_name”)</code>.
<code>imd_drop_cols()</code> returns a character vector of source
columns to remove.

## See Also

<code>read_repertoires()</code>,
<code>make_default_preprocessing()</code>, <code>imd_schema()</code>

## Examples

``` r
library("immundata")

imd_rename_cols("10x")
```

    #>   v_call   d_call   j_call   d_call    locus 
    #> "v_gene" "d_gene" "j_gene" "d_gene"  "chain"

``` r
# Includes c(v_call = "v_gene", locus = "chain").

head(imd_drop_cols("10x"), 3)
```

    #> [1] "full_length" "is_cell"     "contig_id"

``` r
# Expected result:
#   "full_length" "is_cell" "contig_id"

# Keep the 10x `contig_id` column while dropping the other default columns.
columns_to_drop <- setdiff(imd_drop_cols("10x"), "contig_id")
custom_preprocessing <- list(
  exclude_columns = make_exclude_columns(columns_to_drop),
  filter_nonproductive = make_productive_filter(
    truthy = c("TRUE", "true", "1")
  )
)
```
