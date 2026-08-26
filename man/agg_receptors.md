

# Group AIRR sequence rows into receptors

[**Source code**](https://github.com/immunomind/immundata/tree/main/R/operations_agg_receptors.R#L102)

## Description

<code>agg_receptors()</code> is a low-level function used during AIRR
data ingestion. It decides which sequence rows represent the same
biological receptor and adds package-standard identifiers and counts to
the input table.

A receptor can be one chain or a pair of chains from the same cell. The
<code>schema</code> argument defines which sequence features and loci
make two receptors identical.

This function works with a prepared duckplyr table and returns a
duckplyr table. It does not accept or return an ImmunData object. Most
analysis workflows should provide the same arguments to
<code>read_repertoires()</code>, which calls
<code>agg_receptors()</code> during import.

## Usage

<pre><code class='language-R'>agg_receptors(
  dataset,
  schema,
  barcode_col = NULL,
  count_col = NULL,
  locus_col = NULL,
  umi_col = NULL,
  verbose = getOption("immundata.verbose", TRUE)
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="dataset">dataset</code>
</td>
<td>
A duckplyr table containing AIRR sequence data, with one row per chain
or bulk clonotype. It must contain the columns named in
<code>schema</code> and in any of <code>barcode_col</code>,
<code>count_col</code>, <code>locus_col</code>, and <code>umi_col</code>
that are supplied.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>

Definition of receptor identity. Supply either:

<ul>
<li>

A character vector naming the features that must match, such as
<code>c(“v_call”, “j_call”, “junction_aa”)</code>.

</li>
<li>

An object created by <code>make_receptor_schema()</code>. Its
<code>features</code> define chain identity, while its optional
<code>chains</code> select one locus or define a pair of loci.

</li>
</ul>
A schema can contain at most two chain entries. Use syntax such as
<code>c(“IGH”, “IGL|IGK”)</code> to accept either IGH-IGL or IGH-IGK
pairs.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="barcode_col">barcode_col</code>
</td>
<td>
Name of the column containing cell barcodes. Supply this for single-cell
data. <code>umi_col</code> is then also required, and
<code>count_col</code> cannot be supplied. If <code>imd_filename</code>
is present, identical barcode values from different source files are
treated as different cells. The default is <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="count_col">count_col</code>
</td>
<td>
Name of the column containing non-negative abundance values in bulk
repertoire data. These values are copied to <code>imd_n_chains</code>.
<code>count_col</code> cannot be used together with
<code>barcode_col</code>. The default is <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="locus_col">locus_col</code>
</td>
<td>
Name of the column containing loci such as <code>“TRA”</code>,
<code>“TRB”</code>, or <code>“IGH”</code>. It is required when
<code>schema</code> specifies one or more chains. The column is renamed
to the standard name <code>locus</code> when necessary. The default is
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="umi_col">umi_col</code>
</td>
<td>
Name of the column containing per-chain UMI or read counts. It is
required when <code>barcode_col</code> is supplied and is used to choose
one chain when a cell contains several chains from the same locus. The
default is <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
Whether to print information about the selected processing mode and
loci. Defaults to <code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

The receptor features are the columns that define the identity of one
chain. Two chains with the same values in all feature columns receive
the same receptor identity in a single-chain analysis. Common features
include V gene, J gene, and CDR3 amino acid sequence.

The function supports three input modes:

<ul>
<li>

<strong>Uncounted sequence table:</strong> If neither
<code>barcode_col</code> nor <code>count_col</code> is supplied, every
input row is treated as one observed chain. A synthetic barcode is
created for each row, and <code>imd_n_chains</code> is set to
<code>1</code>.

</li>
<li>

<strong>Bulk repertoire:</strong> If <code>count_col</code> is supplied,
every input row receives a synthetic barcode and its abundance is copied
to <code>imd_n_chains</code>.

</li>
<li>

<strong>Single-cell repertoire:</strong> If <code>barcode_col</code> is
supplied, rows are grouped by cell. <code>umi_col</code> is required and
<code>imd_n_chains</code> is set to <code>1</code> for every retained
cell-chain observation.

</li>
</ul>

When one chain is specified in <code>schema</code>, only that locus is
retained. If a cell contains several chains from that locus, the row
with the highest value in <code>umi_col</code> is retained. If the
highest values are tied, the first row is retained.

When two chains are specified, only cells containing both requested loci
are retained. The selected chains are paired by barcode, and both rows
receive the same <code>imd_receptor_id</code>. Cells with incomplete
pairs are excluded.

A relaxed pair such as <code>c(“IGH”, “IGL|IGK”)</code> requires IGH and
exactly one of the two alternative light-chain loci. Cells containing
both IGL and IGK are excluded.

Numeric <code>imd_receptor_id</code> values identify receptors within
the returned table. The particular number assigned to a receptor is not
a biological identifier and may change when the data are aggregated
again.

## Value

A duckplyr table containing the retained input rows and these
package-standard columns:

<ul>
<li>

<code>imd_receptor_id</code>: links rows that belong to the same
receptor.

</li>
<li>

<code>imd_barcode</code>: contains the input cell barcode, or a
synthetic row-level barcode for uncounted and bulk data.

</li>
<li>

<code>imd_chain_id</code>: identifies an individual retained chain row.

</li>
<li>

<code>imd_n_chains</code>: contains <code>1</code> for uncounted and
single-cell data, or the value from <code>count_col</code> for bulk
data.

</li>
<li>

<code>imd_count</code>: initialized to <code>0</code>; receptor counts
are calculated later by <code>agg_repertoires()</code>.

</li>
</ul>

## See Also

<code>read_repertoires()</code>, <code>make_receptor_schema()</code>,
<code>agg_repertoires()</code>, ImmunData
