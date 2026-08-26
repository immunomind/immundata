# immundata 0.1.0

This release introduces manifests as the input-file annotation interface and
makes repertoire, strata, and provenance state more explicit and reliable.

## Breaking changes

* Renamed the input repertoire metadata interface to avoid confusion with the
  `metadata.json` snapshot file. `read_metadata()` is replaced by
  `read_manifest()`. In `read_repertoires()`, use `manifest`,
  `manifest_file_col`, and `path = "<manifest>"` instead of `metadata`,
  `metadata_file_col`, and `path = "<metadata>"`. The default manifest file
  column is now `"file"` rather than `"File"`.
* `read_repertoires()` now uses `repertoire_schema = "<auto>"` by default. This
  creates one repertoire per input file, or one per manifest row when paths are
  supplied by a manifest. Set `repertoire_schema = NULL` to retain the previous
  behavior of leaving repertoires undefined.
* `agg_strata()` now uses the argument names `schema` and `prefix` instead of
  `by` and `strata_name_prefix`.
* Removed the `ImmunData$metadata` accessor. Use `idata$repertoires` for the
  repertoire definitions and summaries, and use manifests for annotations
  associated with input repertoire files.
* For extension developers, `imd_schema("metadata_filename")` is now
  `imd_schema("manifest_filename")`, and the unused `imd_files()$receptors`
  entry has been removed.

## New features and improvements

* `read_repertoires()` now works approximately 60 times faster by 
  combining CSV, TSV, and compressed text inputs into
  one temporary Parquet file before processing by default. This avoids repeated
  text scans in downstream duckplyr queries while retaining original input
  paths in provenance. Use `prematerialize = FALSE` to disable it or
  `prematerialize_folder` to select the temporary storage directory. I recommend you
  to use it pretty much always.
* Added `read_manifest()` for CSV, TSV, TXT, and in-memory manifests. It infers
  common delimiters, resolves file-relative paths, validates file availability,
  and adds normalized source paths for joining to repertoire data. The special
  `repertoire_schema = "<manifest>"` value defines repertoires from all manifest
  columns.
* Promoted strata to first-class `ImmunData` state. Objects now expose
  `schema_strata` and a `$strata` table; `agg_strata()` and `rename_strata()`
  update this state, and snapshots persist and restore it.
* Added grouped mutation through `.by` in `mutate_immundata()` and
  `dplyr::mutate()` methods for `ImmunData`, including a duckplyr-compatible
  fallback for grouped summary expressions.
* Added `conflicts = c("error", "replace")` to the annotation functions.
  Existing annotation columns are protected by default, while intentional
  replacement is allowed for columns that do not define core `ImmunData`
  state.
* `mutate()`, `compute()`, and annotation operations now preserve repertoire,
  strata, and provenance state when the biological grouping has not changed.
  Filtering and downsampling rebuild affected repertoire and strata summaries
  and retain existing stratum labels when possible.
* Added consistent progress control to manifest reading, repertoire ingestion
  and aggregation, and snapshot reading and writing. Use `verbose = FALSE` for
  individual calls or `options(immundata.verbose = FALSE)` globally.
* Snapshot metadata now stores repertoire and strata definitions and validates
  them against the Parquet annotation columns when loading. Older metadata
  formats remain readable and are upgraded in memory when necessary.
* Provenance now includes derived artifact locations (`artifacts_root` and
  `artifacts_path`) associated with the project home and current snapshot.

## Bug fixes

* Corrected repertoire-level cell and receptor counts for paired-chain data and
  prevented chain rows from inflating `n_barcodes`, `n_receptors`, receptor
  proportions, and repertoire-occurrence counts.
* Made `imd_repertoire_id` assignment deterministic by ordering repertoire
  schema values before assigning identifiers.
* Scoped single-cell barcodes by source filename during chain selection and
  pairing, preventing identical barcode strings from different input files from
  being treated as the same cell.
* Sequence filters now retain every chain belonging to a matched receptor,
  including exact, regular-expression, Hamming, and Levenshtein matching.
  Distance calculations no longer use the k-mer prefilter, and temporary DuckDB
  table names are unique across repeated operations.
* Fixed downsampling with DuckDB 1.5 and later, preserved annotation columns in
  bulk count mode, retained provenance and strata state, and made `n = 1` mean
  an absolute sampling depth of one.
* Hardened ingestion against duplicate manifest paths, negative bulk counts,
  missing argument columns, and collisions between custom and canonical locus
  columns.
* Prevented `mutate()` and annotation replacement from overwriting system,
  receptor-schema, repertoire-schema, or strata-defining columns.
* Fixed Windows path handling in the test and example infrastructure.

## Documentation and maintenance

* Reworked the package documentation around biological units, lazy duckplyr
  workflows, ingestion, aggregation, filtering, annotation, snapshots, and
  provenance, and moved website generation to altdoc.
* Removed the unused ggplot2 dependency and raised the minimum supported dplyr
  version to 1.2.1.