

# Display the contents and biological definitions of ImmunData

[**Source code**](https://github.com/immunomind/immundata/tree/main/R/operations_print.R#L51)

## Description

Use <code>print()</code> to inspect the receptor table, chain
annotations, and biological schemas stored in an ImmunData object.

Use this method for a quick overview after reading, filtering, or
aggregating repertoire data. It displays the units available in the
object: receptors, chain rows, repertoires, and strata. It also shows
the feature and chain definitions used to construct receptors.

Printing is read-only. It does not collect the complete dataset into R
and does not change the original object. The object is returned
invisibly so it can still be assigned or used in a pipeline.

## Usage

<pre><code class='language-R'>## S3 method for class 'ImmunData'
print(x, ...)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
An ImmunData object to display.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
Additional arguments. Currently not used.
</td>
</tr>
</table>

## Details

A section is shown only when that information is available. An object
without repertoire definitions, for example, has no repertoire schema or
repertoire summary section. Duckplyr prints a preview of large tables
rather than every row.

## Value

<code>x</code>, invisibly. The displayed output is a human-readable
overview; no data are modified.

## See Also

ImmunData, <code>dplyr::collect()</code>, <code>dplyr::count()</code>

## Examples

``` r
library("immundata")

library(immundata)

options(immundata.verbose = FALSE)
idata <- get_test_idata()

print(idata)
```

    #> # A duckplyr data frame: 3 variables
    #>    imd_receptor_id cdr3_aa       v_call     
    #>              <int> <chr>         <chr>      
    #>  1            1274 ASFPVLSPYNEQF TRBV28*01  
    #>  2             432 ASRAGAGTGELF  TRBV19*01  
    #>  3             433 ASSPGQGLDTQY  TRBV18*01  
    #>  4             837 ASRWGTEAF     TRBV19*01  
    #>  5            1275 ASRLGPNNEQF   TRBV7-2*01 
    #>  6             434 ASSYSEGVIYGYT TRBV6-5*01 
    #>  7             435 SATGTSGEREQF  TRBV20-1*01
    #>  8               1 ASSQSDRVRQPQH TRBV5-6*01 
    #>  9               2 ASSYGPQEGYT   TRBV6-5*01 
    #> 10               3 AISQGQGDTDTQY TRBV10-3*01
    #> # ℹ more rows

    #> # A duckplyr data frame: 39 variables
    #>    sequence_id repertoire_id locus sequence        clone_id v_call d_call j_call
    #>    <chr>       <chr>         <chr> <chr>              <dbl> <chr>  <chr>  <chr> 
    #>  1 M67|4056924 M64-005       TCRB  AGCGCTTCTCCCTG…    36382 TRBV2… NA     TRBJ2…
    #>  2 M67|4056938 M64-005       TCRB  AATCCTTTCCTCTC…        2 TRBV1… TRBD2… TRBJ2…
    #>  3 M67|4056951 M64-005       TCRB  CCAGCATCCTGAGG…     7245 TRBV1… TRBD1… TRBJ2…
    #>  4 M67|4057099 M64-005       TCRB  AATCCTTTCCTCTC…        4 TRBV1… TRBD2… TRBJ1…
    #>  5 M67|4057149 M64-005       TCRB  TCTGCAGAGAGGAC…        5 TRBV7… NA     TRBJ2…
    #>  6 M67|4057174 M64-005       TCRB  ATTTCCCGCTGAGG…        6 TRBV6… TRBD1… TRBJ1…
    #>  7 M67|4057180 M64-005       TCRB  TCTGACAGTGACCA…        7 TRBV2… TRBD2… TRBJ2…
    #>  8 M67|4057263 M64-005       TCRB  TAACTATAGCTCTG…        8 TRBV5… TRBD1… TRBJ1…
    #>  9 M67|4057351 M64-005       TCRB  CGCTCAGGCTGCTG…        9 TRBV6… NA     TRBJ1…
    #> 10 M67|4057402 M64-005       TCRB  ATCAAAGACAGAGG…       10 TRBV1… TRBD1… TRBJ2…
    #> # ℹ more rows
    #> # ℹ 31 more variables: productive <lgl>, fwr1 <chr>, cdr1 <chr>, fwr2 <chr>,
    #> #   cdr2 <chr>, fwr3 <chr>, cdr3 <chr>, fwr4 <chr>, fwr1_aa <chr>,
    #> #   cdr1_aa <chr>, fwr2_aa <chr>, cdr2_aa <chr>, fwr3_aa <chr>, cdr3_aa <chr>,
    #> #   fwr4_aa <chr>, junction <chr>, junction_aa <chr>, counts <dbl>,
    #> #   imd_filename <chr>, imd_barcode <chr>, imd_chain_id <int>,
    #> #   imd_receptor_id <int>, imd_n_chains <dbl>, file <chr>, Therapy <chr>, …

    #> # A tibble: 2 × 4
    #>   imd_repertoire_id imd_filename                          n_barcodes n_receptors
    #>               <int> <chr>                                      <int>       <int>
    #> 1                 1 /home/runner/work/_temp/Library/immu…        955         871
    #> 2                 2 /home/runner/work/_temp/Library/immu…        947         867

``` r
# Expected output contains these sections:
#   ImmunData
#   Receptors
#   Annotations
#   Receptor schema
#   Repertoire schema
#   List of repertoires

# `Receptors` previews distinct biological receptor definitions.
# `Annotations` previews the retained chain rows and sample information.
# The schema sections explain how receptors and repertoires were defined.
```
