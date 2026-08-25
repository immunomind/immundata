

# Group repertoires into biological strata

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_agg_strata.R#L77)

## Description

Use <code>agg_strata()</code> to place sample repertoires into
biological comparison groups, such as treatment arms, tissues, or
disease groups.

Use this function after <code>agg_repertoires()</code> when several
repertoires should be analysed as one group. A <em>stratum</em> contains
every repertoire with the same value, or the same combination of values,
in <code>schema</code>.

The unit being grouped is a whole repertoire. The function returns a new
ImmunData object. The original object is not changed.

## Usage

<pre><code class='language-R'>agg_strata(idata, schema, prefix = "Strata")
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An ImmunData object with repertoires already defined. Use
<code>agg_repertoires()</code> first if the object does not contain
repertoires.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
A non-empty character vector. One or more repertoire-level columns that
define a stratum. For example, use <code>“Therapy”</code> for treatment
arms or <code>c(“Tissue”, “Disease”)</code> for each tissue and disease
combination. The columns must be present in
<code>idata$repertoires</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="prefix">prefix</code>
</td>
<td>
A non-empty character string. Prefix for the automatic stratum labels.
The default, <code>“Strata”</code>, produces labels such as
<code>“Strata1”</code> and <code>“Strata2”</code>. You can use
<code>rename_strata()</code> to assign meaningful labels later instead.
</td>
</tr>
</table>

## Details

If <code>schema</code> contains several columns, a separate stratum is
created for each observed combination. For example, <code>c(“Tissue”,
“Therapy”)</code> can define separate blood and tumour strata within
each treatment arm.

Calling <code>agg_strata()</code> again replaces the existing strata
with groups defined by the new <code>schema</code>.

## Value

A new ImmunData object in which every repertoire belongs to one stratum.
The <code style="white-space: pre;">$strata</code> table lists the
strata, their defining biological values, and their automatic labels.
Repertoire definitions and summary statistics are preserved.

## Identifiers and storage

<code>imd_strata_id</code> is an internal identifier and can change when
strata are rebuilt. It is added to the repertoire table and to the
underlying chain annotations. <code>strata_name</code> is stored only in
the smaller repertoire and strata tables.

Calling <code>agg_repertoires()</code> again rebuilds the repertoires,
so it removes the existing strata. Call <code>agg_strata()</code> again
after redefining repertoires.

## See Also

<code>agg_repertoires()</code>, <code>rename_strata()</code>, ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Define sample repertoires using the biological metadata in the test data
idata <- get_test_idata() |>
  agg_repertoires(c("Response", "Therapy"))

# Group the sample repertoires into treatment arms
treatment_groups <- idata |>
  agg_strata(schema = "Therapy")

treatment_groups$repertoires |>
  select(Therapy, Response, imd_strata_id, strata_name) |>
  arrange(imd_strata_id)
```

    #> # A tibble: 2 × 4
    #>   Therapy Response imd_strata_id strata_name
    #> * <chr>   <chr>            <int> <chr>      
    #> 1 CAR-T   PR                   1 Strata1    
    #> 2 ICI     FR                   2 Strata2

``` r
# Expected result:
#   Therapy Response imd_strata_id strata_name
#   CAR-T   PR                   1 Strata1
#   ICI     FR                   2 Strata2

# Each repertoire is now assigned to its treatment stratum. Any additional
# repertoire with the same Therapy value would receive the same stratum ID.
```
