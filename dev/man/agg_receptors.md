

# Aggregates AIRR data into receptors

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_agg_receptors.R#L88)

## Description

Processes a table of immune receptor sequences (chains or clonotypes) to
identify unique receptors based on a specified schema. It assigns a
unique identifier (<code>imd_receptor_id</code>) to each distinct
receptor signature and returns an annotated table linking the original
sequence data to these receptor IDs.

This function is a core component used within
<code>read_repertoires()</code> and handles different input data
structures:

<ul>
<li>

Simple tables (no counts, no cell IDs).

</li>
<li>

Bulk sequencing data (using a count column).

</li>
<li>

Single-cell data (using a barcode/cell ID column). For single-cell data,
it can perform chain pairing if the schema specifies multiple chains
(e.g., TRA and TRB).

</li>
</ul>

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
A <code>duckplyr_df</code> containing AIRR data. Must include columns
specified in <code>schema</code> and potentially
<code>barcode_col</code>, <code>count_col</code>,
<code>locus_col</code>, <code>umi_col</code>. Expected
<code>idata$annotations</code>, support for <code>ImmunData</code> will
probably be added later.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>

Defines how a unique receptor is identified. Can be:

<ul>
<li>

A character vector of column names representing receptor features (e.g.,
<code>c(“v_call”, “j_call”, “junction_aa”)</code>).

</li>
<li>

A list created by <code>make_receptor_schema()</code>, specifying both
<code>features</code> (character vector) and optionally
<code>chains</code> (character vector of locus names like
<code>“TRA”</code>, <code>“TRB”</code>, <code>“IGH”</code>,
<code>“IGK”</code>, <code>“IGL”</code>, max length 2). Specifying
<code>chains</code> triggers filtering by locus and enables pairing
logic if two chains are given.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="barcode_col">barcode_col</code>
</td>
<td>
Character(1). The name of the column containing cell identifiers
(barcodes). Required for single-cell processing and chain pairing. When
the internal source-file column <code>imd_filename</code> is present,
single-cell identity is scoped by both source file and barcode during
aggregation. Default: <code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="count_col">count_col</code>
</td>
<td>
Character(1). The name of the column containing counts (e.g., UMI counts
for bulk, clonotype frequency). Used for bulk data processing. Default:
<code>NULL</code>. Cannot be specified if <code>barcode_col</code> is
set.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="locus_col">locus_col</code>
</td>
<td>
Character(1). The name of the column specifying the chain locus (e.g.,
"TRA", "TRB"). Required if <code>schema</code> includes
<code>chains</code> for filtering or pairing. Default:
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="umi_col">umi_col</code>
</td>
<td>
Character(1). The name of the column containing UMI counts. Required for
single-cell data (<code>barcode_col</code> is set). Used to select the
most abundant chain within each barcode and, for paired schemas, within
each barcode/locus group when multiple chains are present. Default:
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
Logical(1). Whether to print informative messages. Defaults to
<code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

The function performs the following main steps:

<ol>
<li>

<strong>Validation:</strong> Checks inputs, schema validity, and
existence of required columns.

</li>
<li>

<strong>Schema Parsing:</strong> Determines receptor features and target
chains from <code>schema</code>.

</li>
<li>

<strong>Locus Filtering:</strong> If <code>schema$chains</code> is
provided, filters the dataset to include only rows matching the
specified locus/loci.

</li>
<li>

<strong>Processing Logic (based on <code>barcode_col</code> and
<code>count_col</code>):</strong>

<ul>
<li>

<strong>Simple Table/Bulk (No Barcodes):</strong> Assigns unique
internal barcode/chain IDs. Identifies unique receptors based on
<code>schema$features</code>. Calculates <code>imd_chain_count</code> (1
for simple table, from <code>count_col</code> for bulk).

</li>
<li>

<strong>Single-Cell (Barcodes Provided):</strong> Uses
<code>barcode_col</code> for <code>imd_barcode_id</code>.

<ul>
<li>

<strong>Single Chain:</strong> (<code>length(schema$chains) \<=
1</code>). Identifies unique receptors based on
<code>schema$features</code>. Uses <code>umi_col</code> to keep one
chain per barcode when needed. <code>imd_chain_count</code> is 1.

</li>
<li>

<strong>Paired Chain:</strong> (<code>length(schema$chains) ==
2</code>). Requires <code>locus_col</code> and <code>umi_col</code>.
Filters chains within each cell/locus group based on max
<code>umi_col</code>. Creates paired receptors by joining the two
specified loci for each cell based on <code>schema$features</code> from
both. Assigns a unique <code>imd_receptor_id</code> to each
<em>pair</em>. <code>imd_chain_count</code> is 1 (representing the chain
record).

</li>
</ul>
</li>
</ul>
</li>
<li>

<strong>Output:</strong> Returns an annotated data frame containing
original columns plus internal identifiers
(<code>imd_receptor_id</code>, <code>imd_barcode_id</code>,
<code>imd_chain_id</code>) and counts (<code>imd_chain_count</code>).

</li>
</ol>

Internal column names are typically managed by
<code>immundata:::imd_schema()</code>.

## Value

A <code>duckplyr_df</code> (or data frame) representing the annotated
sequences. This table links each original sequence record (chain) to a
defined receptor and includes standardized columns:

<ul>
<li>

<code>imd_receptor_id</code>: Integer ID unique to each distinct
receptor signature.

</li>
<li>

<code>imd_barcode_id</code>: Integer ID unique to each cell/barcode (or
row if no barcode).

</li>
<li>

<code>imd_chain_id</code>: Integer ID unique to each input row (chain).

</li>
<li>

<code>imd_chain_count</code>: Integer count associated with the chain (1
for SC/simple, from <code>count_col</code> for bulk). This output is
typically assigned to the
<code style="white-space: pre;">$annotations</code> field of an
<code>ImmunData</code> object.

</li>
</ul>

## See Also

<code>read_repertoires()</code>, <code>make_receptor_schema()</code>,
ImmunData
