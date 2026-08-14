

# Convert an immunarch Object into an ImmunData Dataset

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_immundata_conversion.R#L37)

## Description

The <code>from_immunarch()</code> function takes an
<strong>immunarch</strong> object (as returned by
<code>immunarch::repLoad()</code>), writes each repertoire to a TSV file
with an added internal filename column in a specified folder, and then
imports those files into an <strong>ImmunData</strong> object via
<code>read_repertoires()</code>.

## Usage

<pre><code class='language-R'>from_immunarch(
  imm,
  output_folder,
  schema = c("CDR3.aa", "V.name"),
  temp_folder = file.path(tempdir(), "temp_folder")
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="imm">imm</code>
</td>
<td>

A list returned by <code>immunarch::repLoad()</code>, typically
containing:

<ul>
<li>

<strong><code>data</code></strong>: a named list of
<code>data.frame</code>s, one per repertoire.

</li>
<li>

<strong><code>meta</code></strong>: (optional) a <code>data.frame</code>
of sample metadata.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="output_folder">output_folder</code>
</td>
<td>
Path to the output directory where the resulting ImmunData Parquet files
will be stored. This directory will be created if it does not already
exist.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
Character vector of column names that together define unique receptors
(for example, <code>c(“CDR3.aa”, “V.name”, “J.name”)</code>).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="temp_folder">temp_folder</code>
</td>
<td>
Path to a directory where intermediate TSV files will be written.
Defaults to <code>file.path(tempdir(), “temp_folder”)</code>.
</td>
</tr>
</table>

## Value

An ImmunData object containing all repertoires from the input immunarch
object, with data saved under <code>output_folder</code>.

## See Also

<code>read_repertoires()</code>, <code>read_immundata()</code>,
ImmunData

## Examples

``` r
library("immundata")

imm <- immunarch::repLoad("/path/to/your/files")
idata <- from_immunarch(imm,
  schema = c("CDR3.aa", "V.name"),
  temp_folder = tempdir(),
  output_folder = "/path/to/immundata_out"
)
```
