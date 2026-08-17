

# Create or validate a receptor schema object

## Description

Helper functions for defining and validating the <code>schema</code>
used by <code>agg_receptors()</code> to identify unique receptors.

<code>make_receptor_schema()</code> creates a schema list object.
<code>assert_receptor_schema()</code> checks if an object is a valid
schema list and throws an error if not.
<code>test_receptor_schema()</code> checks if an object is a valid
schema list or a character vector (which <code>agg_receptors</code> can
also accept) and returns <code>TRUE</code> or <code>FALSE</code>.

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
Character vector. Column names defining the features of a single
receptor chain (e.g., V gene, J gene, CDR3 sequence).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="chains">chains</code>
</td>
<td>
Optional character vector (max length 2). Locus names (e.g.,
<code>“TRA”</code>, <code>“TRB”</code>) to filter by or pair. If
<code>NULL</code> or length 1, only filtering occurs. If length 2,
pairing logic is enabled in <code>agg_receptors()</code>. Default:
<code>NULL</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
An object to test or assert as a valid schema. Can be a list created by
<code>make_receptor_schema</code> or a character vector (for
<code>test_receptor_schema</code>).
</td>
</tr>
</table>

## Value

<code>make_receptor_schema</code> returns a list with elements
<code>features</code> and <code>chains</code>.
<code>assert_receptor_schema</code> returns <code>TRUE</code> invisibly
if valid, or stops execution. <code>test_receptor_schema</code> returns
<code>TRUE</code> or <code>FALSE</code>.
