

# Aggregate AIRR data into repertoires

## Description

Groups the annotation table of an <code>ImmunData</code> object by
user-specified columns to define distinct <em>repertoires</em> (e.g.,
based on sample, donor, time point). It then calculates summary
statistics both per-repertoire and per-receptor within each repertoire.

Calculated <strong>per repertoire</strong>:

<ul>
<li>

<code>n_barcodes</code>: Total number of unique cells/barcodes within
the repertoire (sum of <code>imd_chain_count</code>, effectively summing
unique cells if input was SC, or total counts if input was bulk).

</li>
<li>

<code>n_receptors</code>: Number of unique receptors
(<code>imd_receptor_id</code>) found within the repertoire.

</li>
</ul>

Calculated <strong>per annotation row</strong> (receptor within
repertoire context):

<ul>
<li>

<code>imd_count</code>: Total count of a specific receptor
(<code>imd_receptor_id</code>) within the specific repertoire it belongs
to in that row (sum of relevant <code>imd_chain_count</code>).

</li>
<li>

<code>imd_proportion</code>: The proportion of the repertoire’s total
<code>n_barcodes</code> accounted for by that specific receptor
(<code>imd_count / n_barcodes</code>).

</li>
<li>

<code>n_repertoires</code>: The total number of distinct repertoires
(across the entire dataset) in which this specific receptor
(<code>imd_receptor_id</code>) appears.

</li>
</ul>

These statistics are added to the annotation table, and a summary table
is stored in the <code style="white-space: pre;">$repertoires</code>
slot of the returned object.

## Usage

<pre><code class='language-R'>agg_repertoires(
  idata,
  schema = "repertoire_id",
  verbose = getOption("immundata.verbose", TRUE)
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An <code>ImmunData</code> object, typically the output of
<code>read_repertoires()</code> or <code>read_immundata()</code>. Must
contain the <code style="white-space: pre;">$annotations</code> table
with columns specified in <code>schema</code> and internal columns like
<code>imd_receptor_id</code> and <code>imd_chain_count</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
Character vector. Column name(s) in <code>idata$annotations</code> that
define a unique repertoire. For example, <code>c(“SampleID”)</code> or
<code>c(“DonorID”, “TimePoint”)</code>. Columns must exist in
<code>idata$annotations</code>. Default: <code>“repertoire_id”</code>
(assumes such a column exists).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
Logical(1). Reserved for consistency with other aggregation functions.
Defaults to <code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

The function operates on the <code>idata$annotations</code> table:

<ol>
<li>

<strong>Validation:</strong> Checks <code>idata</code> and existence of
<code>schema</code> columns. Removes any pre-existing repertoire summary
columns to prevent duplication.

</li>
<li>

<strong>Repertoire Definition:</strong> Groups annotations by the
<code>schema</code> columns. Calculates total counts
(<code>n_barcodes</code>) per group. Assigns a unique integer
<code>imd_repertoire_id</code> to each distinct repertoire group. This
forms the initial <code>repertoires_table</code>.

</li>
<li>

<strong>Receptor Counts & Proportion:</strong> Calculates the sum of
<code>imd_chain_count</code> for each receptor within each repertoire
(<code>imd_count</code>). Calculates the proportion
(<code>imd_proportion</code>) of each receptor within its repertoire.

</li>
<li>

<strong>Repertoire & Receptor Stats:</strong> Counts unique receptors
per repertoire (<code>n_receptors</code>, added to
<code>repertoires_table</code>). Counts the number of distinct
repertoires each unique receptor appears in
(<code>n_repertoires</code>).

</li>
<li>

<strong>Join Results:</strong> Joins the calculated
<code>imd_count</code>, <code>imd_proportion</code>, and
<code>n_repertoires</code> back to the annotation table based on
repertoire columns and <code>imd_receptor_id</code>.

</li>
<li>

<strong>Return New Object:</strong> Creates and returns a <em>new</em>
<code>ImmunData</code> object containing the updated
<code style="white-space: pre;">$annotations</code> table (with the
added statistics) and the
<code style="white-space: pre;">$repertoires</code> slot populated with
the <code>repertoires_table</code> (containing <code>schema</code>
columns, <code>imd_repertoire_id</code>, <code>n_barcodes</code>,
<code>n_receptors</code>).

</li>
</ol>

The original <code>idata</code> object remains unmodified. Internal
column names are typically managed by
<code>immundata:::imd_schema()</code>.

## Value

A <strong>new</strong> <code>ImmunData</code> object. Its
<code style="white-space: pre;">$annotations</code> table includes the
added columns (<code>imd_repertoire_id</code>, <code>imd_count</code>,
<code>imd_proportion</code>, <code>n_repertoires</code>). Its
<code style="white-space: pre;">$repertoires</code> slot contains the
summary table linking <code>schema</code> columns to
<code>imd_repertoire_id</code>, <code>n_barcodes</code>, and
<code>n_receptors</code>.

## See Also

<code>read_repertoires()</code> (which can call this function),
ImmunData class.

## Examples

``` r
library("immundata")

# Assume 'idata_raw' is an ImmunData object loaded via read_repertoires
# but *without* providing 'repertoire_schema' initially.
# It has $annotations but $repertoires is likely NULL or empty.
# Assume idata_raw$annotations has columns "SampleID" and "TimePoint".

# Define repertoires based on SampleID and TimePoint
idata_aggregated <- agg_repertoires(idata_raw, schema = c("SampleID", "TimePoint"))

# Explore the results
print(idata_aggregated)
print(idata_aggregated$repertoires)
print(head(idata_aggregated$annotations)) # Note the new columns
```
