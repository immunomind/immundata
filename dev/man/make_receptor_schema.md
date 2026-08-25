

# Define which chain observations form the same receptor

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/utils_schema.R#L106)

## Description

Use <code>make_receptor_schema()</code> to define a biological receptor
from sequence features and one or two receptor chains.

Use this function when reading single-cell data with one selected chain,
when pairing chains such as TRA-TRB, or when accepting alternative light
chains such as IGK or IGL. The unit being defined is the receptor.
Creating a schema does not change any data or an existing ImmunData
object.

## Usage

<pre><code class='language-R'>make_receptor_schema(features, chains = NULL)

assert_receptor_schema(schema)

test_receptor_schema(schema)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="features">features</code>
</td>
<td>
A non-empty character vector. Column names containing the chain fields
that must match, such as <code>c(“junction_aa”, “v_call”,
“j_call”)</code>. Use names as they appear after any input-column
renaming.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="chains">chains</code>
</td>
<td>
A character vector of length one or two, or <code>NULL</code>. Use one
value, such as <code>“TRB”</code>, to keep one chain; two values, such
as <code>c(“TRA”, “TRB”)</code>, to define a strict pair; or the
<code>“IGK|IGL”</code> syntax in the second value to accept either
alternative. The default is <code>NULL</code>, which does not select
loci.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
A non-empty character vector or receptor-schema list. An object to
check. A schema created by <code>make_receptor_schema()</code> is
accepted. A character vector supplies feature names for a chain-agnostic
schema.
</td>
</tr>
</table>

## Value

<code>make_receptor_schema()</code> returns a list with character
elements <code>features</code> and <code>chains</code>;
<code>chains</code> is <code>NULL</code> when loci are not selected.
<code>assert_receptor_schema()</code> returns <code>TRUE</code> for
accepted input and otherwise stops with an error.
<code>test_receptor_schema()</code> returns one logical value.

## When observations are the same receptor

<code>features</code> names the fields that define chain identity.
Common choices are the CDR3 amino acid sequence, V gene, and J gene. Two
observations represent the same receptor only when the relevant chain
loci and every selected feature match.

<ul>
<li>

With one chain, such as <code>chains = “TRB”</code>, only that locus is
used. Two TRB observations are the same receptor when all their selected
feature values match.

</li>
<li>

With a strict pair, such as <code>chains = c(“TRA”, “TRB”)</code>,
chains are first paired within each cell barcode. Receptors from two
cells are the same only when every selected TRA feature and every
selected TRB feature match.

</li>
<li>

With an alternative second chain, such as <code>chains = c(“IGH”,
“IGK|IGL”)</code>, each receptor must contain IGH and exactly one of IGK
or IGL. Cells containing both IGK and IGL are excluded. The light-chain
locus and all selected heavy- and light-chain features must match for
two observations to be the same receptor.

</li>
</ul>

A barcode determines which chains belong to the same cell; it does not
by itself define receptor identity across cells. During single-cell
import, <code>read_repertoires()</code> uses <code>umi_col</code> to
choose one chain when a cell contains several observations from the same
locus.

Use <code>chains = NULL</code> for chain-agnostic bulk or pre-filtered
data. In that case, only the values in <code>features</code> define
receptor identity.

## Validate a schema

<code>assert_receptor_schema()</code> stops with an error if
<code>schema</code> is not accepted. Use it inside another function when
invalid input must stop the calculation.
<code>test_receptor_schema()</code> returns one <code>TRUE</code> or
<code>FALSE</code> value and is useful in conditional code.

## Backend and storage

A receptor schema is a small R list containing <code>features</code> and
<code>chains</code>. It stores no sequence data.
<code>read_repertoires()</code> and <code>agg_receptors()</code> apply
the schema to chain observations using duckplyr.

## See Also

<code>read_repertoires()</code>, <code>agg_receptors()</code>,
<code>imd_schema()</code>

## Examples

``` r
library("immundata")

# Single-chain TCR: compare TRB observations by CDR3, V gene, and J gene.
trb_schema <- make_receptor_schema(
  features = c("junction_aa", "v_call", "j_call"),
  chains = "TRB"
)
trb_schema
```

    #> $features
    #> [1] "junction_aa" "v_call"      "j_call"     
    #> 
    #> $chains
    #> [1] "TRB"

``` r
# Expected result:
#   $features: "junction_aa" "v_call" "j_call"
#   $chains:   "TRB"

# Paired alpha-beta TCR: all selected fields must match on both TRA and TRB.
ab_tcr_schema <- make_receptor_schema(
  features = c("junction_aa", "v_call", "j_call"),
  chains = c("TRA", "TRB")
)
ab_tcr_schema
```

    #> $features
    #> [1] "junction_aa" "v_call"      "j_call"     
    #> 
    #> $chains
    #> [1] "TRA" "TRB"

``` r
# The result defines one receptor as a matched TRA-TRB pair from one cell.

# BCR: require IGH and accept either an IGK or IGL light chain.
bcr_schema <- make_receptor_schema(
  features = c("junction_aa", "v_call", "j_call"),
  chains = c("IGH", "IGK|IGL")
)
bcr_schema
```

    #> $features
    #> [1] "junction_aa" "v_call"      "j_call"     
    #> 
    #> $chains
    #> [1] "IGH"     "IGK|IGL"

``` r
# The result accepts IGH-IGK and IGH-IGL receptors, while keeping the two
# light-chain loci biologically distinct.

test_receptor_schema(bcr_schema)
```

    #> [1] TRUE

``` r
# Expected result: TRUE
```
