

# ImmunData: A Unified Structure for Immune Receptor Repertoire Data

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/#L)

## Description

<code>ImmunData</code> is an abstract R6 class for managing and
transforming immune receptor repertoire data. It supports flexible
backends (e.g., Arrow, DuckDB, dbplyr) and lazy evaluation, and provides
tools for filtering, aggregation, and receptor-to-repertoire mapping.

## Public fields

<dl>
<dt>
<code>schema_receptor</code>
</dt>
<dd>
A named list describing how to interpret receptor-level data. This
includes the fields used for aggregation (e.g., <code>CDR3</code>,
<code>V_gene</code>, <code>J_gene</code>), and optionally unique
identifiers for each receptor row. Used to ensure consistency across
processing steps.
</dd>
<dt>
<code>schema_repertoire</code>
</dt>
<dd>
A character vector defining how barcodes or annotations should be
grouped into repertoires. This may include sample-level metadata (e.g.,
<code>sample_id</code>, <code>donor_id</code>) used to define unique
repertoires.
</dd>
<dt>
<code>schema_strata</code>
</dt>
<dd>
A character vector naming repertoire-level columns used to group
repertoires into strata.
</dd>
</dl>

## Active bindings

<dl>
<dt>
<code>receptors</code>
</dt>
<dd>
Accessor for the dynamically-created table with receptors.
</dd>
<dt>
<code>annotations</code>
</dt>
<dd>
Accessor for the annotation-level table (<code>.annotations</code>).
</dd>
<dt>
<code>repertoires</code>
</dt>
<dd>
Get a table of repertoires and their basic statistics.
</dd>
<dt>
<code>strata</code>
</dt>
<dd>
Get one row per stratum with its schema values and label.
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

Creates a new <code>ImmunData</code> object. This constructor expects
receptor-level and barcode-level data, along with a receptor schema
defining aggregation and identity fields.

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
A character vector specifying the receptor schema (e.g., aggregate
fields, ID columns).
</dd>
<dt>
<code>annotations</code>
</dt>
<dd>
A cell/barcode-level dataset mapping barcodes to receptor rows.
</dd>
<dt>
<code>repertoires</code>
</dt>
<dd>
A repertoire table, created inside the body of agg_repertoires.
</dd>
<dt>
<code>provenance</code>
</dt>
<dd>
Internal provenance metadata for snapshot lineage.
</dd>
<dt>
<code>strata</code>
</dt>
<dd>
An optional strata table containing the strata ID, name, and columns
defining the strata schema.
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

<code>read_repertoires()</code>, <code>read_immundata()</code>
