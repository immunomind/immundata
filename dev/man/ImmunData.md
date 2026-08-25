

# ImmunData: A data structure for storing adaptive immune receptor repertoire data

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/#L)

## Description

<code>ImmunData</code> stores adaptive immune receptor repertoire (AIRR)
data and the rules used to turn observed sequences or cells into data
units for analysis. Think AnnData or SeuratObject, but for immune
repertoires.

You work with an <code>ImmunData</code> object after importing bulk or
single-cell AIRR-seq data. The major idea behind <code>ImmunData</code>
is that because sequencing provides only information about sequences
and, for single-cell data, cell barcodes, the your responsibility is to
determine, which sequences you want to treat as the same receptor,
repertoire, or stratum (group of repertoires). You define these analysis
units with schemas. A schema is a stored set of column names and
chain-selection rules that tells <code>ImmunData</code> how to group
observations. Those definitions are kept inside <code>ImmunData</code>
to ensure that downstream functions count, filter, and compare the same
units consistently. Repertoire and strata schemas can be changed later
to re-aggregate repertoires differently, e.g., merge receptors from
different clusters into per-patient clusters. Receptor schema is fixed
once and for all, so if you want to work with a different receptor
definition, e.g., use "CDR3aa + V gene" instead of just "CDR3aa" as a
definiton for a unique receptor, you will need to create a separate
<code>ImmunData</code> object.

<code>ImmunData</code> is immutable, meaning that functions that
transform an <code>ImmunData</code> object return a new object, and the
original object is not changed. Due to multiple optimisations on the
backend, it does not mean that you re-create the whole dataset each time
you run a, let’s stay, a filter. However, it does affect analysis
workflow significantly. You can read about it more on the website and in
tutorials.

## From observed data to analysis units

<code>ImmunData</code> connects observed records to user-defined
analysis units:

<ul>
<li>

A <strong>chain observation</strong> is an observed receptor-chain
sequence, such as a TRA, TRB, or IGH sequence. Chain observations form
the main table.

</li>
<li>

A <strong>barcode</strong> is an observed identifier for a cell in
single-cell data. It links chains found in the same cell.

</li>
<li>

A <strong>receptor</strong> is a virtual analysis unit that you define.
For example, you may define it by CDR3 sequence alone, by CDR3 and V
gene, or as a paired TRA-TRB receptor. The receptor schema records which
chain features and loci must match for observations to receive the same
receptor identifier.

</li>
<li>

A <strong>repertoire</strong> is a virtual collection of receptors that
you define from annotation columns. For example, one repertoire may
contain all receptors from one sample, or from one donor at one time
point.

</li>
<li>

A <strong>stratum</strong> is a virtual collection of repertoires for a
comparison. For example, one stratum may contain all repertoires from
one treatment arm.

</li>
</ul>

These definitions do not change the observed sequences. They determine
how observations are grouped and counted during analysis. The resulting
hierarchy is <code style="white-space: pre;">chain observations and
barcodes -\> receptors -\> repertoires -\> strata</code>.

## Inspect and transform an object

Print an object for a compact overview. Use
<code style="white-space: pre;">$receptors</code> for the receptor
table, <code style="white-space: pre;">$repertoires</code> for one
summary row per repertoire, and
<code style="white-space: pre;">$strata</code> for one row per stratum.
Most analysis functions accept the complete <code>ImmunData</code>
object directly.

Common transformations include:

<ul>
<li>

<code>filter_immundata()</code> to keep selected chains, cells, or
receptors;

</li>
<li>

<code>mutate_immundata()</code> to calculate annotation columns;

</li>
<li>

<code>annotate()</code> to add external biological information;

</li>
<li>

<code>agg_repertoires()</code> to define repertoires; and

</li>
<li>

<code>agg_strata()</code> to group repertoires into strata.

</li>
</ul>

## Create an object

Create an <code>ImmunData</code> object with
<code>read_repertoires()</code>, or reopen a saved object with
<code>read_immundata()</code>. Do not call the
<code style="white-space: pre;">$new()</code> constructor in analysis
code. Direct construction is reserved for package developers.

## Lazy data and storage

The chain-level table uses duckplyr and can remain on disk. Filtering,
mutation, and aggregation stay lazy when possible, so large datasets do
not need to be loaded fully into R memory. Downstream analysis functions
in the <code>immunarch</code> package are designed to accept lazy
<code>ImmunData</code> objects. Pass the object directly; you usually do
not need to call <code>dplyr::collect()</code>. Collect data only when
another function explicitly requires an in-memory data frame or when you
want to inspect a small table in R.

Objects created by <code>read_repertoires()</code> are backed by files
in their output folder. Keep that folder while you use the object. Use
<code>write_immundata()</code> to save a transformed object and
<code>read_immundata()</code> to reopen it.

## Public fields

<dl>
<dt>
<code>schema_receptor</code>
</dt>
<dd>
A named list defining the virtual receptor unit. The
<code>features</code> element names the chain columns used to group
observations, such as CDR3 sequence and V gene. The <code>chains</code>
element selects one chain or a paired set of chains.
</dd>
<dt>
<code>schema_repertoire</code>
</dt>
<dd>
A character vector naming annotation columns whose unique combinations
define one repertoire, such as <code>sample_id</code> or
<code>c(“donor_id”, “timepoint”)</code>. It is <code>NULL</code> when
repertoires have not been defined.
</dd>
<dt>
<code>schema_strata</code>
</dt>
<dd>
A character vector naming repertoire-level columns whose unique
combinations define one stratum, such as <code>treatment</code>. It is
<code>NULL</code> when strata have not been defined.
</dd>
</dl>

## Active bindings

<dl>
<dt>
<code>receptors</code>
</dt>
<dd>
A derived duckplyr table of distinct receptors. For a paired receptor,
the selected chain features are shown side by side.
</dd>
<dt>
<code>annotations</code>
</dt>
<dd>
The lazy duckplyr table of retained chain observations and their
biological annotations. For most tasks, pass the complete
<code>ImmunData</code> object to a transformation function or use
<code>collect(idata)</code> to inspect this table in memory.
</dd>
<dt>
<code>repertoires</code>
</dt>
<dd>
A small table with one row per repertoire, the columns that define it,
and summary statistics such as <code>n_barcodes</code> and
<code>n_receptors</code>. It is <code>NULL</code> when repertoires have
not been defined.
</dd>
<dt>
<code>strata</code>
</dt>
<dd>
A small table with one row per stratum, its label, and the
repertoire-level columns that define it. It is <code>NULL</code> when
strata have not been defined.
</dd>
<dt>
<code>provenance</code>
</dt>
<dd>

Read-only named list describing the snapshot origin and storage context
carried by this object. Retrieve the complete list with
<code>idata$provenance</code>, or one field with, for example,
<code>idata$provenance$current_path</code>. The fields are:

<ul>
<li>

<code>home_path</code>: project home used for managed snapshots and
artifacts. The original ingestion snapshot is stored directly in this
folder; it is <code>NULL</code> for an object with no persisted home.

</li>
<li>

<code>current_path</code>: exact folder of the most recently loaded or
written snapshot. Transformations preserve this source path until the
transformed object is written as another snapshot; it is
<code>NULL</code> for an object that has never been loaded from or
written to disk.

</li>
<li>

<code>snapshot_root</code>: derived managed-snapshot root,
<code>home_path/snapshots</code>, or <code>NULL</code> when
<code>home_path</code> is <code>NULL</code>.

</li>
<li>

<code>artifacts_root</code>: derived project-level root for optional
external tool outputs, <code>home_path/artifacts</code>, or
<code>NULL</code> when <code>home_path</code> is <code>NULL</code>.

</li>
<li>

<code>artifacts_path</code>: derived namespace for artifacts associated
with the most recently loaded or written snapshot. It is
<code>artifacts_root/root</code> for the original ingestion,
<code style="white-space: pre;">artifacts_root/\<tag\>/vNNN</code> for a
managed snapshot, and
<code style="white-space: pre;">artifacts_root/by-id/\<snapshot_id\></code>
for a detached explicit snapshot. External tools can append
<code style="white-space: pre;">\<tool\>/\<run\></code> and create that
directory; artifact contents are not part of <code>ImmunData</code>.
Write a transformed object as a new snapshot before storing artifacts
that should be associated with the transformed data.

</li>
<li>

<code>snapshot_id</code>: unique identifier generated when the snapshot
is written; <code>NULL</code> for an in-memory object that has never
been written.

</li>
<li>

<code>lineage</code>: ordered list of ingestion and snapshot events
leading to the current snapshot.

</li>
</ul>
The accessor is read-only; assigning to <code>idata$provenance</code> is
an error.
</dd>
</dl>

## Methods

<h4>
Public methods
</h4>
<ul>
<li>

<a href="#method-ImmunData-initialize"><code>ImmunData$new()</code></a>

</li>
<li>

<a href="#method-ImmunData-clone"><code>ImmunData$clone()</code></a>

</li>
</ul>
<hr>

<a id="method-ImmunData-initialize"></a>

<h4>
<code>ImmunData$new()</code>
</h4>

Low-level constructor for package developers. Analysis code must create
an <code>ImmunData</code> object with <code>read_repertoires()</code> or
reopen one with <code>read_immundata()</code>.

<h5>
Usage
</h5>

<pre>ImmunData\$new(
  schema,
  annotations,
  repertoires = NULL,
  provenance = NULL,
  strata = NULL
)</pre>

<h5>
Arguments
</h5>

<dl>
<dt>
<code>schema</code>
</dt>
<dd>
A character vector or named list. A character vector names the features
used to define a chain-agnostic receptor. A named list is created by
<code>make_receptor_schema()</code> and can also select receptor chains.
</dd>
<dt>
<code>annotations</code>
</dt>
<dd>
A duckplyr table. It contains retained chain observations, receptor
identifiers, and biological annotations.
</dd>
<dt>
<code>repertoires</code>
</dt>
<dd>
A data frame or <code>NULL</code>. It contains one row per repertoire
and its summary statistics and is usually created by
<code>agg_repertoires()</code>.
</dd>
<dt>
<code>provenance</code>
</dt>
<dd>
A list or <code>NULL</code>. It contains internal storage and snapshot
history.
</dd>
<dt>
<code>strata</code>
</dt>
<dd>
A data frame or <code>NULL</code>. It contains one row per stratum, its
label, and the repertoire-level columns that define it.
</dd>
</dl>

<hr>

<a id="method-ImmunData-clone"></a>

<h4>
<code>ImmunData$clone()</code>
</h4>

The objects of this class are cloneable with this method.

<h5>
Usage
</h5>

<pre>ImmunData\$clone(deep = FALSE)</pre>

<h5>
Arguments
</h5>

<dl>
<dt>
<code>deep</code>
</dt>
<dd>
Whether to make a deep clone.
</dd>
</dl>

## See Also

<code>read_repertoires()</code>, <code>read_immundata()</code>,
<code>write_immundata()</code>, <code>agg_repertoires()</code>,
<code>agg_strata()</code>, <code>filter_immundata()</code>,
<code>mutate_immundata()</code>, <code>annotate()</code>

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Load the small dataset included with immundata, then define one repertoire
# for each treatment-response group.
idata <- get_test_idata() |>
  agg_repertoires(schema = "Response")

idata$repertoires |>
  select(Response, n_barcodes, n_receptors) |>
  arrange(Response)
```

    #> # A tibble: 2 × 3
    #>   Response n_barcodes n_receptors
    #> * <chr>         <dbl>       <int>
    #> 1 FR              955         871
    #> 2 PR              947         867

``` r
# Expected result:
#   Response n_barcodes n_receptors
#   FR              955         871
#   PR              947         867

# Under the current receptor definition, the full-response (FR) repertoire
# contains 955 chain observations grouped into 871 receptor units.

# Keep only the full-response repertoire. filter() returns a new object.
fr_only <- idata |>
  filter(Response == "FR")

tibble(
  original_repertoires = nrow(idata$repertoires),
  filtered_repertoires = nrow(fr_only$repertoires)
)
```

    #> # A tibble: 1 × 2
    #>   original_repertoires filtered_repertoires
    #>                  <int>                <int>
    #> 1                    2                    1

``` r
# Expected result:
#   original_repertoires filtered_repertoires
#                      2                    1
# The original object still contains both repertoires.
```
