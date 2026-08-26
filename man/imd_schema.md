

# Get a standard ImmunData column name

[**Source code**](https://github.com/immunomind/immundata/tree/main/R/globals.R#L169)

## Description

Use <code>imd_schema()</code> when code needs the standard column name
for an <code>ImmunData</code> identifier or calculated value, such as
the cell barcode, receptor identifier, repertoire identifier, count, or
proportion.

Use this helper in reusable analysis code or package extensions instead
of writing an internal name such as <code>“imd_barcode”</code> directly.
It only returns names; it does not inspect or change an ImmunData
object.

## Usage

<pre><code class='language-R'>imd_schema(key = NULL)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="key">key</code>
</td>
<td>
A character string or <code>NULL</code>. One schema key, for example
<code>“barcode”</code>, <code>“receptor”</code>,
<code>“repertoire”</code>, <code>“count”</code>, or
<code>“proportion”</code>. Use <code>NULL</code>, the default, to return
all available keys and column names.
</td>
</tr>
</table>

## Value

If <code>key</code> is supplied, one character string containing the
standard column name. If <code>key = NULL</code>, a named list of all
schema keys and column names.

## See Also

<code>make_receptor_schema()</code>, <code>imd_rename_cols()</code>,
ImmunData

## Examples

``` r
library("immundata")

imd_schema("barcode")
```

    #> [1] "imd_barcode"

``` r
# Expected result: "imd_barcode"

imd_schema("receptor")
```

    #> [1] "imd_receptor_id"

``` r
# Expected result: "imd_receptor_id"

# Use a returned name for programmatic selection.
barcode_column <- imd_schema("barcode")
get_test_idata() |>
  dplyr::collect() |>
  dplyr::select(dplyr::all_of(barcode_column)) |>
  head(2)
```

    #> # A tibble: 2 × 1
    #>   imd_barcode
    #>   <chr>      
    #> 1 S1_1       
    #> 2 S1_2
