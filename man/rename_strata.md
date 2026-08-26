

# Give biological strata readable labels

[**Source code**](https://github.com/immunomind/immundata/tree/main/R/operations_agg_strata.R#L236)

## Description

Use <code>rename_strata()</code> to replace automatic stratum labels
with names that are clear in figures and result tables, such as
<code>“Control”</code>, <code>“Treated”</code>, or <code>“Tumour
tissue”</code>.

Use this function after <code>agg_strata()</code> when labels such as
<code>“Strata1”</code> do not describe the biological groups. The unit
being changed is the stratum label. Stratum membership and the
repertoires, receptors, cells, and chains remain unchanged.

The function returns a new ImmunData object. The original object is not
changed.

## Usage

<pre><code class='language-R'>rename_strata(
  idata,
  names,
  unnamed = c("error", "auto", "keep"),
  auto_prefix = "Strata"
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An ImmunData object with strata already created by
<code>agg_strata()</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="names">names</code>
</td>
<td>

A named character vector or a data frame. New labels matched to
<code>imd_strata_id</code>. Supply either:

<ul>
<li>

a named character vector, such as <code>c(“1” = “Control”, “2” =
“Treated”)</code>; or

</li>
<li>

a data frame with columns <code>imd_strata_id</code> and
<code>strata_name</code>.

</li>
</ul>
Every new label must be non-empty and unique.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="unnamed">unnamed</code>
</td>
<td>
A character string. What to do when <code>names</code> does not include
every stratum. The default, <code>“error”</code>, asks for a complete
mapping. Use <code>“auto”</code> to generate labels for missing strata
or <code>“keep”</code> to preserve their current labels.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="auto_prefix">auto_prefix</code>
</td>
<td>
A non-empty character string. Prefix used to generate labels when
<code>unnamed = “auto”</code>. The default is <code>“Strata”</code>.
</td>
</tr>
</table>

## Details

The names of a named character vector are the stratum IDs, not the
current labels. Inspect <code>idata$strata</code> to find the ID for
each biological group.

The mapping cannot contain unknown or repeated IDs, and the resulting
labels must be unique across strata.

## Value

A new ImmunData object with the requested labels in its
<code style="white-space: pre;">$strata</code> and
<code style="white-space: pre;">$repertoires</code> tables. All
biological group assignments and repertoire summaries are preserved.

## Storage details

The readable <code>strata_name</code> is stored in the repertoire and
strata tables. The underlying chain annotations keep only
<code>imd_strata_id</code>, so renaming a stratum does not rewrite or
regroup chain-level data.

## See Also

<code>agg_strata()</code>, <code>agg_repertoires()</code>

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Create treatment strata for the sample repertoires in the test data
treatment_groups <- get_test_idata() |>
  agg_repertoires(c("Response", "Therapy")) |>
  agg_strata(schema = "Therapy")

# Replace automatic labels with names suitable for a figure
labeled_groups <- treatment_groups |>
  rename_strata(
    names = c("1" = "CAR-T arm", "2" = "ICI arm")
  )

labeled_groups$strata |>
  select(Therapy, imd_strata_id, strata_name) |>
  arrange(imd_strata_id)
```

    #> # A tibble: 2 × 3
    #>   Therapy imd_strata_id strata_name
    #> * <chr>           <int> <chr>      
    #> 1 CAR-T               1 CAR-T arm  
    #> 2 ICI                 2 ICI arm

``` r
# Expected result:
#   Therapy imd_strata_id strata_name
#   CAR-T               1 CAR-T arm
#   ICI                 2 ICI arm

# Only the labels changed. Each sample repertoire remains in the same
# treatment stratum.
```
