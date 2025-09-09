<div align="center">
  <h2>🦋 <code>immundata</code> --- <strong>Data layer for large-scale multi-modal immune repertoires in R</strong></h2>
</div>

---

<div align="center">
  <a href="https://github.com/immunomind">
    <img alt="Ecosystem: ImmunoMind"
         src="https://img.shields.io/badge/ecosystem-ImmunoMind-orange?style=flat-square">
  </a>
  <a href="https://cran.r-project.org/package=immundata">
    <img alt="CRAN Version"
         src="https://www.r-pkg.org/badges/version-ago/immundata?style=flat-square">
  </a>
  <a href="https://www.r-pkg.org/pkg/immundata">
    <img alt="CRAN Downloads (all time)"
         src="https://cranlogs.r-pkg.org/badges/grand-total/immundata">
  </a>
  <a href="https://www.r-pkg.org/pkg/immundata">
    <img alt="CRAN Downloads (last week)"
         src="https://cranlogs.r-pkg.org/badges/last-week/immundata">
  </a>
  <a href="https://github.com/immunomind/immunarch/issues">
    <img alt="GitHub Issues"
         src="https://img.shields.io/github/issues/immunomind/immundata?style=flat-square">
  </a>
</div>

<p align="center">
  <a href="https://immunomind.github.io/docs/tutorials/single-cell/">Tutorials</a>
  |
  <a href="https://immunomind.github.io/immundata/reference">API reference</a>
  |
  <a href=https://immunomind.github.io/docs/>Ecosystem</a>
  |
  Publication (coming soon...)
</p>

---

`immundata` introduces the `ImmunData` data structure – think [AnnData](https://github.com/scverse/anndata) or [SeuratObject](https://github.com/satijalab/seurat-object/) but for immune repertoires – so you can have:

- **Single source of truth:** store tens of millions of immune receptors plus metadata in one place;

- **Multi-modality:** compute receptor- and repertoire-level statistics leveraging single-cell, spatial, immunogenicity or any other annotations;

- **Immunomics at scale:** work seamlessly with datasets that don't fit in memory;

- **Reproducibility by design:** run the same workflow on a laptop, server, or cloud instance.

---

## 🤔 Why `immundata`?

Modern immunomics no longer ends at a couple of FASTQ files and a bar plot:

- We now blend bulk AIRR-seq, single-cell V(D)J + GEX, spatial transcriptomics, clinical metadata and public databases -- often inside the same analysis notebook;

- Pipelines that handle gigabytes today face deca-gigabytes after the next experiment;

- The same immune repertoire dataset must power multiple plots, dashboards, deep learning models and be reproducible months (years, ideally) later.

`immundata` brings you a unified data layer for large-scale single-cell, spatial and bulk immunomics in R.
It is the data-engineering backbone powered by [Arrow](https://arrow.apache.org/docs/r/), [DuckDB](https://duckdb.org/), and [duckplyr](https://duckplyr.tidyverse.org/).
`immundata` lets you scale, mix and, ultimately, analyse annotated AIRR data without rewriting your biology workflow from scratch each time the dataset grows 10×.

---

> [!WARNING]
> `immundata` is still in the **0.x** series. Until we reach 1.0.0, breaking changes may appear in any minor/patch update (e.g. 0.2.1 → 0.3.0). When you attach the package, sometimes you'll see startup messages summarising
> the most important changes – please read them. If something that used to work suddenly fails, check the updated
> documentation (`?function_name`) first.
>   
> **Tip:** if your analysis depends on a specific behaviour, pin the
> exact version with `renv` or use `pak` for installation:
> ```r
> pak::pkg_install("immunomind/immundata@0.2.1")
> ```  
> I'll keep publishing tagged releases with full docs so you can always
> roll back if needed.

---

> [!IMPORTANT]
> This README is huge. I'm not kidding. Please consider using navigation.

- 🤔 [Why `immundata`?](#-why--immundata-)
- 📦 [Installation](#-installation)
- ⚡ [Quick Start](#-quick-start)
- 🧬 [Workflow Explained](#-workflow-explained)
- 💾 [Ingestion](#-ingestion)
  - [Load AIRR data](#load-airr-data)
  - [Working with metadata table files](#working-with-metadata-table-files)
  - [Receptor schema](#receptor-schema)
  - [Repertoire schema](#repertoire-schema)
  - [Pre‑ and post‑processing strategies](#pre--and-post‑processing-strategies)
  - [Managing output & intermediate ImmunData files](#managing-output--intermediate-immundata-files)
  - [Writing ImmunData objects to disk](#writing-immundata-objects-to-disk)
- 🧿 [ImmunData Object](#-immundata-object)
- 🛠 [Transformation](#-transformation)
  - [Filter](#filter)
  - [Annotate](#annotate)
  - [Modify](#modify)
- 📈 [Analysis](#-analysis)
  - [Basic analysis using `immundata`](#basic-analysis-using--immundata-)
  - [Advanced analysis using `immunarch`](#advanced-analysis-using--immunarch-)
- 🧩 [Use Cases](#-use-cases)
  - [Bulk -- RepSeq, AIRRSeq](#bulk---repseq-airrseq)
  - [Paired-chain -- scVDJseq or other technologies](#paired-chain---scvdjseq-or-other-technologies)
  - [Single-cell -- scRNAseq, scVDJseq, scTCRseq, scBCRseq](#single-cell---scrnaseq-scvdjseq-sctcrseq-scbcrseq)
  - [Spatial -- spatial transcriptomics and cell coordinates](#spatial---spatial-transcriptomics-and-cell-coordinates)
  - [Annotate immune receptors using external AIRR databases](#annotate-immune-receptors-using-external-airr-databases)
  - [Immunogenicity -- run external tools such as TCRdist to annotate ImmunData](#immunogenicity----run-external-tools-such-as-tcrdist-to-annotate-immundata)
  - [Hybrid datasets](#hybrid-datasets)
    - [Multi-locus data](#multi-locus-data)
    - [Multiple contigs for TCR](#multiple-contigs-for-tcr)
    - [BCR-heavy chains with multiple light chains](#bcr-heavy-chains-with-multiple-light-chains)
    - [Bulk and single-cell data integration](#bulk-and-single-cell-data-integration)
- 🧠 [Advanced Topics](#-advanced-topics)
- 🏷 [About](#-about)
  - [Citation](#citation)
  - [License](#license)
  - [Author and contributors](#author-and-contributors)
  - [Commercial usage](#commercial-usage)
- 🤔 [FAQ](#-faq)

---

## 📦 Installation

### Prerequisites

Before installing any release or pre-release version of `immundata`, please install `pak` that will simplify the installation of any package, not just `immundata`:

```r
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
```

More info if needed is available on [pak website](https://pak.r-lib.org/#arrow_down-installation).

### Install the latest version

To install the latest release of `immundata`, simply run:

```r
pak::pkg_install("immunomind/immundata")
```

Mind that this will install the package from our GitHub instead of CRAN. This method is much preferred due to limitations of CRAN and reliance on other packages, which are distributed via `pak` as well.

### Other installation options

We will periodically release `immundata` on CRAN. To install it from CRAN, run 

```r
pak::pkg_install("immundata")
```

If you are willing to try unstable yet bleeding edge features, or if there are some hot fix for your open GitHub ticket, please install the development version:

```r
pak::pkg_install("immunomind/immundata@dev")
```

---

## ⚡ Quick Start

Use the immune repertoire data packaged with `immundata` for quick dive.
Replace `system.file` calls with your local file paths to run the code on your data.

```r
library(immundata)

# Metadata table with additional sample-level information
md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")

# Two sample files
samples <- c(
  system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"), 
  system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )

# Read the metadata table
md <- read_metadata(md_path)

# Pass the file paths and the metadata table to the function to read the dataset into R
imdata <- read_repertoires(path          = samples,
                           schema        = c("cdr3_aa", "v_call"),
                           metadata      = md,
                           output_folder = "./immundata-quick-start")

# Print the resultant object in the detailed yet manageable format
imdata

# Check the folder immundata created - this is where your dataset resides now
list.files("./immundata-quick-start")

# Read sections below for data analysis
```

---

## 🧬 Workflow Explained

`immundata` splits the workflow into two clear phases:

1. **Ingestion** – convert your AIRR files into a special format saved on disk, and then read them to a tidy `immundata::ImmunData` object  

2. **Transformation**  – explore, annotate, filter and compute on that object

Before we go into more details for each of the phase, there are three straightforward yet essential `immundata` concepts to keep in mind. These concepts set it apart from data-frame-based AIRR libraries. By extension, the concepts affect how you would work with and even *think* about the data analysis in other packages such as `immunarch` which use `immundata` as a backbone for computations.

### Concepts

1. **Units: chain -> barcode -> receptor**

    | Term               | In plain English                                                                                         | How **immundata** represents it                                                                                                             | **Role**                                                              |
    | ------------------ | -------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------- |
    | **Chain**          | A single V(D)J transcript (e.g. *TRA* or *IGH*) coming from one read or contig.                          | One row in the physical table `idata$annotations`; retains `locus`, `cdr3`, `umis`/`reads` and other crucial rearrangement characteristics. | **Raw data unit** – atomic building block.                            |
    | **Barcode / Cell** | The droplet (10x), spot (Visium) or well a chain was captured in.                                        | Column `imd_barcode`.                                                                                                                       | **Physical bundle** – groups chains that share a capture compartment. |
    | **Receptor**       | The biological receptor you analyse: a single chain **or** a paired set (αβ, Heavy-Light) from one cell. | Virtual table `idata$receptors`; unique ID `imd_receptor_id`.                                                                               | **Logical unit** – minimal object for AIRR statistics.                |
    | **Repertoire**     | A set of receptors grouped by sample, donor, cluster, etc.                                               | Physical table `idata$repertoires`; unique ID `imd_repertoire_id`; grouping columns you choose.                                             | **Aggregate unit** – higher-level grouping for comparative analysis.  |

    **Chain** is one V(D)J rearranged molecule / contig / chemistry read (e.g. a single TRA, TRB, IGH, IGL). This is a minimally possible data unit, a building block of everything.
    In case of single-chain data, chain is the same as barcode. Never changes after ingest; you can always drill back to the exact sequence.
    
    **Barcode** is a physical container that stores zero, one, or many chains. In single‑cell it's a droplet == cell; in bulk it's the same as assembled "clonotype"; in spatial it's a spot. Barcode sometimes equals to cell. It is a biological unit that "stores" relevant biological data and uses for aggregation of same chains and computing counts of same receptors coming from different barcodes. Inherits any per‑cell / per‑sample metadata you add.
    
    **Receptor** is a logical unit. A logical grouping of chains that you want to treat as one biological receptor. This is a minimal unit for AIRR statistics. There are two components to it: receptor features and receptor chains, alltogether comprising a receptor schema that you define in order to do downstream analysis. Receptor features are usually CDR3 amino acid sequences or CDR3 amino acid sequences plus Variable gene segment. Receptor chains can be: single chain, α+β pair, heavy+light pair, or even all chains sharing the same CDR3/V/J.
    
    To summarise: chains are how `immundata` stores the information, barcodes bundle chains together, and receptors are the minimal units on which repertoire statistics are computed.

2. **Aggregation: defining receptors and repertoires**

    The moment data leave an AIRR-assembly tool such as **Cell Ranger**, you are handed an ocean of individual V(D)J chains, yet every biological question you care about is phrased in terms of receptors ("this αβ TCR") or repertoires ("all receptors from donor A on day 30"). As with "receptors as logical units", the underlying assumption the second concept is based upon is *researchers work with rearrangements but think in receptors*. `immundata` formalises the climb from raw chains to those higher-order concepts through **controlled aggregation** – explicit, user-defined rules that transform data without obscuring their origin.

    The function `agg_receptors()` lets you declare what *one receptor* means in your study. You choose a schema – perhaps "pair chains that share a barcode and have complementary α and β loci" or "group every IGH with whatever IGL shares the same CDR3 amino-acid sequence." The function re-aggregates the data and returns a new `ImmunData` object, so you keep the previous receptor definition intact; every receptor now has a stable identifier and can be traced back to its constituent chains and barcode. There is no need to touch the downstream pipeline – just change the input.

    The function `agg_repertoires()` states how receptors should be bundled into biologically meaningful cohorts: all receptors from a biopsy, from a therapy responder, from a single-cell-defined cluster, or any combination of metadata columns. The result is a physical `idata$repertoires` table with basic statistics (numbers of chains, barcodes, and unique receptors), again preserving direct links to the receptors it aggregates.

    Because these aggregation steps live in your pipeline rather than being buried inside helper functions, they deliver two major pay-offs:

    - **Convenience with rigour:** you can run high-level computations – Jaccard coefficients, diversity indices – knowing that the exact receptor definition is stored alongside the result, so you never mis-specify parameters such as `"cdr3+v"`;
    
    - **Provenance and data lineage by design:** every receptor records every chain it contains, every repertoire records every receptor, and the full recipe is stored in the object's metadata. Six months later – or six reviewers later – you can trace any summary statistic back to the precise chains that produced it, enabling fully reproducible pipelines with no hidden transformations.

3. **Pipeline-based execution: immutability and materialisation**

    The explicit data lineage we talked about in concepts 1 & 2 pays its dividend only if every step is re-playable. That is why `immundata` treats an analysis as a *pipeline of immutable transformations*. Each function returns a fresh `ImmunData` object, leaving the parent untouched; the full chain of objects records how the data travelled from raw chains to final statistics.

    Because immunomics datasets started to regularly outgrow RAM, those objects do **not** live in memory by default. Their tables are persisted as column-compressed Parquet files and "materialised" – pulled into RAM – only when a computation truly needs them, typically to crunch a subset or to emit the final numbers such as repertoire-overlap indices. For a 10 GB dataset that fits in memory, this behaviour is invisible: DuckDB streams the file, you get an in-memory frame, and life goes on. For a 100 GB experiment, the same code still runs; the heavy joins spill to disk, and the intermediate results are cached so downstream steps can reuse them without recomputation.
    
    Thinking in pipelines therefore means two things:
    
    - **Cache what matters:** create intermediate `ImmunData` objects when you hit an expensive step, and write them to disk; the next run can pick up from there. A prime example of this a computing edit distances to some patterns or sequences.
    
    - **Assume re-execution:** any colleague (or future-you on a bigger cluster) should be able to rerun `pipeline.R` end-to-end without interactive tinkering and arrive at the same result byte-for-byte.
    
    All of this engineering should stay behind the curtain. Downstream packages that adopt `immundata` as their backbone should expose high-level verbs such as `compute_diversity()` or `plot_overlap()`; the user need not touch `ImmunData`, DuckDB, or Parquet. In the ideal scenario they never learn that an on-disk database powers their workflow – and they never have to.
    
    Leave the data engineering to the data engineers (and, sadly, bioinformaticians – I feel your pain); keep your focus or the focus of your users on the biology. It is sophisticated enough already.

And now, let's dive into how you work with `immundata`.

### Phase 1: Ingestion

```
      ┌───────┐
      │ files │
      └───────┘
          │
          ▼
   read_metadata()    ──── Read metadata
          │
          ▼ 
  read_repertoires()  ──┬─ Read repertoire files (!)
          │             │      ▼
          │             │  Preprocess
          │             │      ▼
          │             │  Aggregate receptors (!)
          │             │      ▼
          │             │  Postprocess
          │             │      ▼
          │             │  Aggregate repertoires #1
          │             │      ▼
          │             └─ Write data on disk (!)
          ▼
   agg_repertoires()  ──── Aggregate repertoires #2
          │
          ▼
    ┌───────────┐
    │ ImmunData │
    └───────────┘
```

Steps marked with `(!)` are non-optional.

The goal of the **ingestion phase** is to turn a folder of AIRR-seq files into an immutable on-disk `ImmunData` dataset.

  1) **Read metadata:**
  
      `read_metadata()` pulls in any sample- or donor-level information, such as therapy arm, HLA type, age, etc., and stores it in a data frame that we can pass to the main reading functions `read_repertoires`. Attaching this context early means every chain you read later already "knows" which patient or time-point it belongs to.
  
      You can safely skip it if you don't have per-sample pr per-donor metadata.
  
  2) **Read repertoire files:**
  
      `read_repertoires()` streams Parquet/CSV/TSV files straight into DuckDB that powers `ImmunData` objects.
  
  3) **Preprocess:**
  
      During the read step you may pass a `preproc = recipe` argument to `read_repertoires` to preprocess data before aggregating receptors: drop unused columns, strip non-productive sequences, translate field names to the AIRR schema, de-duplicate contigs, etc. Because this logic is declarative, re-runs produce identical results.
  
  4) **Aggregate receptors:**
  
      Receptor schema is how you define a receptor – a logical unit of analysis. The `read_repertoires` collapses chains into receptors accordingly and assigns each a stable unique identifier.
      
  3) **Postprocess:**
  
      A mirror step to **preprocess**: a convenient hook to run QC checks, add derived fields, attach reference-gene annotations, or compute per-chain quality metrics **after** the dataset is ready. You can pass any number of steps which will be executed in a sequential order.
  
  5) **Aggregate repertoires #1:**
  
      If you already know how to group chains into receptors, perhaps by `"Sample"` or `"Donor"` columns from the metadata, you can pass `repertoire_schema = c("Sample")` to `read_repertoires()`. Otherwise, skip and define repertoires later (common in single-cell workflows where you need cluster labels first).
      
  3) **Write data on disk:**
  
      `read_repertoires` always persists what it just built: column-compressed Parquet parts plus a human-readable metadata in JSON. From here on, downstream steps can reopen the dataset instantly without touching the raw AIRR files again.
    
  5) **Aggregate repertoires #2:**
  
      Call `agg_repertoires()` later if you withheld grouping until additional annotations were available, e.g. donor + cell cluster.

### Phase 2: Transformation

```
      ┌───────────┐       ┌────────────────────────────┐
      │ ImmunData │       │ AnnData / Seurat / TCRdist │
      └───────────┘       │ seur@meta.data / adata.obs │
            │             └────────────────────────────┘
            │                             │
            ├─────────────────────────────┘
            │
            ▼
   annotate_immundata()    ──── Import external annotations to ImmunData
            │
            ▼ 
     agg_repertoires()     ──── Aggregate repertoires
            │
            ▼ 
    filter_immundata()     ──── Filter receptors or repertoires
            │
            ▼ 
    mutate_immundata()     ──── Create or modify columns, compute statistics
            │
            │     ┌────────────────┐
            ├────►│ save / plot #1 │
            │     └────────────────┘
            ▼ 
   annotate_immundata()    ──── Annotate ImmunData with the computed statistics
            │
            │     ┌────────────────┐
            ├────►│ save / plot #2 │
            │     └────────────────┘
            │
            ▼
┌────────────────────────┐
│seur@meta.data[:] <- ...│ ──── Export ImmunData annotations
│    adata.obs = ...     │
└────────────────────────┘
```

Transformation is a loop of annotation → modification and computation → visualisation, always producing a new `ImmunData` while leaving the parent intact. That immutability is what turns every notebook into a reproducible pipeline.

  1) **Import external annotations to ImmunData:**
  
      `annotate_immundata()` (or its thin wrappers `annotate_barcodes()` / `annotate_receptors()`) merges labels from Seurat/AnnData/TCRdist/anything that can be expressed as a keyed data frame to the main table, so each chain has a corresponding annotation.
  
  2) **Aggregate repertoires:**
  
      Now that extra labels are present, you might regroup receptors, for example, by donor × cell-state.
  
  3) **Filter receptors or repertoires:**
  
      `filter_immundata()` accepts tidy-verse predicates on chains, receptors, or repertoires.
  
  4) **Create or modify columns, compute statistics:**
  
      On this step, you compute statistics per-repertoire or per-receptor, using input receptor features. There are several scenarios depending on what you try to achieve.
  
      1) use `immunarch` for the most common analysis functions. The package will automatically annotate both **receptors/barcodes/chains** (!) and **repertoires** (!!) if it is possible;
  
      2) simply mutate on the whole dataset using `dplyr` syntax, like compute edit distance to a specific pattern using `mutate_immundata`;
  
      3) more complex compute that requires a function to apply to values and is probably not supported by `duckplyr`. See the [🧠 Advanced Topics](#-advanced-topics) for more details.
      
  4) **Save / plot #1:**
  
      Cache the `ImmunData`. Use `ggplot2` to visualise the statistics, computed from `ImmunData`.
  
  5)  **Annotate ImmunData with the computed statistics:**
  
      `annotate_immundata()` (again) joins the freshly minted statistics back to the canonical dataset.
  
  4) **Save / plot #2:**
  
      Save the `ImmunData` with new annotations to disk. Plot the results of analysis.
  
  6)  **Export ImmunData annotations:**
  
      Write the annotated data back to the cell-level dataset (Seurat / AnnData) for the subsequent analysis. Additionally, you could write the `ImmunData` itself to disk if needed.

---

## 💾 Ingestion

### Load AIRR data

`immundata` provides a flexible system for loading immune receptor repertoire files from different sources -- CSV, TSV and Parquet files, possibly gzipped, with some optionality. The main function for this is `read_repertoires()`. Below are four ways to pass your file paths and one for convering data from existing `immunarch pre-1.0` list objects with `$data` and `$meta`.

  1. **Pass a single file name:**
  
      If you just have **one** AIRR file:

      ```r
      library(immundata)
      
      inp_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
      
      idata <- read_repertoires(
        path   = inp_file,
        schema = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

  2. **Pass a vector of file names:**
  
      For **multiple** files in a vector:

      ```r
      library(immundata)
      
      inp_file1 <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
      inp_file2 <- system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
      
      file_vec <- c(inp_file1, inp_file2)
      
      idata <- read_repertoires(
        path   = file_vec,
        schema = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

      `immundata` automatically merges them (depending on your chosen schema), writes the aggregated data into a single directory of Parquet files, and produces a single-cell `ImmunData` object. Think about it as a huge table instead of smaller multiple repertoire tables.

  3. **Pass a glob pattern:**
  
      If your files follow a consistent naming pattern, you can leverage shell globs:

      ```r
      library(immundata)
      
      folder_with_files <- system.file("extdata/tsv", "", package = "immundata")
      
      glob_files <- paste0(folder_with_files, "sample*.tsv")
      
      print(glob_files)
      # The output is something like "/Library/Frameworks/.../immundata/extdata/tsv/*"
      # Mind the star "*" at the end
      
      # For example, all AIRR files in the 'samples/' folder
      idata <- read_repertoires(
        path   = glob_files,
        schema = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

      Behind the scenes, `read_repertoires()` expands the glob with `Sys.glob(...)`, merges the data, and produces a single `ImmunData`.

  4. **Use a metadata file:**
  
      Sometimes you need more control over the data source (e.g. consistent sample naming, extra columns). In that case:

        1.  **Load metadata** with `read_metadata()`.
        
        2.  **Pass** the resulting data frame to `read_repertoires(path = "<metadata>", ..., metadata = md_table)`. Mind the `"<metadata>"` string we pass to the function. It indicates that we should take file paths from the input metadata table.

      An example code:

      ```r
      library(immundata)
      
      md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
      
      md_table <- read_metadata(md_path)
      
      print(md_table)
      ```
      
      ```
      # The column "File" stores the file paths. If you have a different column name
      # for files, use the `metadata_file_col = "<your column name>"` argument.
      # A tibble: 2 × 5
        File                       Therapy Response Prefix filename
        <chr>                      <chr>   <chr>    <chr>  <chr>   
      1 /.../immundata-/inst/extd… ICI     FR       S1_    /Users/…
      2 /.../immundata-/inst/extd… CAR-T   PR       S2_    /Users/…
      ```
      
      ```r
      idata <- read_repertoires(
        path     = "<metadata>",
        metadata = md_table,
        schema   = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

      This approach **unifies** sample-level metadata (e.g. donor ID, timepoint) with your repertoire data inside a single `ImmunData`.
      
      You can pass the metadata table separately along with the list of files as we did in the previous examples without the "<metadata>" directive, but in that case you would need to check the correctness of all filepaths by yourself. Which could be quite cumbersome, to say the least.
      
      The more information on how to work with metadata files, please read the next section.

  5. **Convert from `immunarch` lists:**
  
      Pass `immunarch` data lists to `from_immunarch()` to create `ImmunData` objects.
  
      ```r
      library(immundata)
      # Install old immunarch:
      # pak::pkg_install("immunomind/immunarch@0.9.1")
      data(immdata, package = "immunarch")
      
      idata <- from_immunarch(
        imm = immdata, 
        schema = c("CDR3.aa", "V.name"), 
        output_folder = "./immdata-test")
        
      print(idata)
      ```

### Working with metadata table files

Metadata tables store the sample-level information. When `immundata` loads the metadata, it annotates every receptor from a given sample (or file) with the corresponding metadata fields. For example, if a sample has "Therapy" = "CAR‑T", all receptors from that sample receive the same "Therapy" value. You can then aggregate receptors by donor, tissue, or any other field and run your analysis on those repertoires (see the next sections for aggregations).

> [!WARNING]
> In the current version, "metadata" and "repertoire schema" is the same, meaning you can't get
> a metadata field to `idata$repertoires` if you haven't define repertoires using that field.
> I will implement it in the next versions; for now, please consider using `dplyr::left_join` to
> merge metadata and the repertoires table together.


```r
library(immundata)

md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
md_table <- read_metadata(md_path)
```

```
Rows: 2 Columns: 4
── Column specification ─────────────────────────────────────────────────────────
Delimiter: "\t"
chr (4): File, Therapy, Response, Prefix

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
ℹ Found 2/2 repertoire files from the metadata on the disk
✔ Metadata parsed successfully
```

```r
print(md_table)
```

```
# A tibble: 2 × 5
  File                       Therapy Response Prefix filename
  <chr>                      <chr>   <chr>    <chr>  <chr>   
1 /.../immundata-/inst/extd… ICI     FR       S1_    /Users/…
2 /.../immundata-/inst/extd… CAR-T   PR       S2_    /Users/…
```

### Receptor schema

`immundata` lets you decide what a receptor means for your study by specifying:

 - Feature columns - which fields make a receptor unique. Usually it is `"cdr3_aa"` and `"v_call"` columns.

 - Chains to keep / pair - e.g. TRA only or a pair TRA + TRB.

If you have only feature columns, you can usually pass the character vector with columns to functions. In a more advanced case with multiple chain data, `immundata` provides a helper function `make_receptor_schema()` for building schemas:

```r
schema <- make_receptor_schema(
  features = c("cdr3_aa", "v_call"),
  chains   = c("TRA", "TRB")
)
```

  1. **Chain-agnostic**

      Used for bulk or pre-filtered immune repertoire data. No filtering by chain data such as TRA or TRB. Each unique combination of features in the schema vector is assigned a unique receptor identifier and counts as a receptor. In the example below, the receptor features are "cdr3_aa" and "v_call" columns - CDR3 amino acid sequence and V gene segment columns respectively.
      
      ```r
      library(immundata)
            
      inp_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
      
      schema <- c("cdr3_aa", "v_call")
            
      idata <- read_repertoires(
        path   = inp_file,
        schema = schema
      )
            
      print(idata)
      ```

  2. **Single-chain**
  
      >[!NOTE]
      > Please note that single-chain option does not (!) remove multiple chains per cell - yet.
      > In other words, you will get multiple receptors per barcode. The paired chain option filter out
      > chains which don't have the max number of reads or umis per barcode. So receptor numbers and sequences
      > could differ significantly.

      Used for paired-chain data such as single-cell data to focus on the analysis of immune receptors with a specific chain. The data is pre-filtered to leave the data units with the specified chain only.
      
      ```r
      library(immundata)
            
      inp_file <- system.file("extdata/single_cell", "lt6.csv.gz", package = "immundata")
      
      schema <- make_receptor_schema(
        features = c("cdr3", "v_call"),
        chains   = "TRA"
      )
      
      idata <- read_repertoires(
        path        = inp_file,
        schema      = schema,
        barcode_col = "barcode",
        locus_col   = "locus",
        preprocess  = make_default_preprocessing("10x")
      )
      
      print(idata)
      ```

  3. **Paired-chain**

      When you want full αβ (or heavy‑light) receptors, immundata can pair two chains that originate from the same barcode and keep, for each locus, the chain with the highest UMI/reads. A single unique receptor identifier is then assigned to the pair. The data is pre-filtered to loci in target chains. Within each barcode×locus the the chain with max umis or reads is selected. Barcodes lacking either chain are dropped from the receptor table.
      
      ```r
      library(immundata)
      
      inp_file <- system.file("extdata/single_cell", "lt6.csv.gz", package = "immundata")
      
      schema <- make_receptor_schema(
        features = c("cdr3", "v_call"),
        chains   = c("TRA", "TRB")
      )
      
      idata <- read_repertoires(
        path        = inp_file,
        schema      = schema,
        barcode_col = "barcode",         # required for pairing
        locus_col   = "locus",           # column that says "TRA" / "TRB"
        umi_col     = "umis",            # choose chain with max UMIs per locus
        preprocess  = make_default_preprocessing("10x")
      )
      
      print(idata)
      ```

Cheat-sheet for arguments to `read_repertoires`:

| Situation                                | `barcode_col` | `locus_col` | `umi_col` | `chains`         |
| ---------------------------------------- | ------------- | ----------- | --------- | ---------------- |
| Bulk data, no locus filtering            | no            | no          | no        | omit / `NULL`    |
| Analyse TRA only                         | **yes**¹      | **yes**     | no        | `"TRA"`          |
| Pair TRA+TRB, pick best chain per cell   | **yes**       | **yes**     | **yes**   | `c("TRA","TRB")` |

¹ If you pass barcodes, they're stored but used for counting only.

### Repertoire schema

To compute repertoire‑level statistics such as gene‑segment usage, the Jaccard coefficient, or the incidence of public receptors, you first need to define a repertoire. In `immundata` a repertoire is simply a group of receptors that share one or more values from annotation columns.

Just like with receptors, you can pass a schema – a character vector of column names – to specify how receptors are grouped into repertoires.

For the bulk data, usually, you rely on the metadata table. It could be useful when you want to aggregate together receptors from the same donor or tissue, and then analyse it. Or you may want to filter out non-responders to analyse the responders only.

> [!NOTE]
> Don't confuse grouping of immune repertoires with grouping in plots.
> When you define an immune repertoire, all the proportions are recomputed, and each receptor assigned
> a unique repertoire identifier for faster computations.
> You create virtual "tables" with immune receptors, and you can work with them separately
> using filters or mutations, despite that the data is still stored as a huge table with all receptors.
> When you plot data, you first compute statistics per defined immune repertoire, and then you
> group it. You can later plot or re‑group the resulting statistics, but the order of operations matters.

The true power of regrouping repertoires opens up when you work with single-cell data.

```r
library(immundata)
      
inp_file <- system.file("extdata/single_cell", "lt6.csv.gz", package = "immundata")
md_file <- system.file("extdata/single_cell", "metadata.tsv", package = "immundata")
md_table <- read_metadata(md_file)
schema <- make_receptor_schema(features = c("cdr3", "v_call"), chains   = c("TRA", "TRB"))

idata <- read_repertoires(
  path        = inp_file,
  schema      = schema,
  metadata    = md_table,
  barcode_col = "barcode",         # required for pairing
  locus_col   = "locus",           # column that says "TRA" / "TRB"
  umi_col     = "umis",            # choose chain with max UMIs per locus
  preprocess  = make_default_preprocessing("10x")
)

cells_file <- system.file("extdata/single_cell", "cells.tsv.gz", package = "immundata")
cells <- readr::read_tsv(cells_file)

# External cell annotations
print(cells)

# Annotate data with cell clusters
idata <- annotate_barcodes(
  idata = idata,
  annotations = cells[c("barcode", "ident")], 
  annot_col = "barcode", 
  keep_repertoires = FALSE
)

# Aggregate repertoires to make cluster-specific repertoires
idata <- idata |> agg_repertoires(schema = "ident")

# Take a look at the $repertoires table
print(idata)
```

### Preprocessing and postprocessing strategies 

> [!CAUTION]
> 🚧 Under construction. 🚧

1. *... in progress ...*

  - removing columns ("on" by default)
  - filtering non productive ("on" by default)

2. **Barcode prefix**

    Provide a column named "Prefix" to the metadata so `make_default_postprocessing()` can automatically add this prefix to barcodes to make barcodes unique in your resultant dataset.

### Managing the output and intermediate ImmunData files

> [!CAUTION]
> 🚧 Under construction. 🚧

By default, `read_repertoires()` writes the created Parquet files into a directory named `immundata_<first filen name>`. Consider passing `output_folder` to `read_repertoires()` if you want to specify the output path.

### Writing ImmunData objects on disk
 
Use `write_immundata` to save ImmunData objects.

Why you might need it - to save intermediate files, e.g., after computing levenshtein distance to specific receptors from the database. In that case, you wouldn't need to recompute the distances each time.

---

## 🧿 ImmunData Object

`ImmunData` object is a self-describing container that holds your immune repertoire dataset. `ImmunData` objects has several slots accessible via `<object>$<slot>`:

- `ImmunData$receptors` – a virtual table created on demand from `$annotations`. One row per receptor as defined by your `$schema_receptor`; guaranteed to have the stable key `imd_receptor_id`. This is the aggregated view into your dataset, meaning that all fields from receptor features (cdr3, v_call) are unique with respect to row, i.e., each row is unique.

- `ImmunData$annotations` – the main table that holds all the data. One row per chain (or per cell barcode in case of single-chained data). Holds every AIRR field (cdr3, v_call, umis, etc.) plus any metadata you imported (sample_id, tissue, distances to patterns).

- `ImmunData$repertoires` – a physical table produced by agg_repertoires(). Each row is a repertoire (sample, donor, cluster) and carries pre-computed counts: number of receptors, barcodes, chains.

- `ImmunData$schema_receptor` – the recipe that says how to collapse chains into receptors: which features make them identical (e.g. cdr3_aa, v_call) and which loci must pair (e.g. α+β, heavy+light, single-chain).

- `ImmunData$schema_repertoire` – the grouping keys used to bundle receptors into repertoires (e.g. sample_id, donor_id, time_point, etc.). Lets you define multiple biological layers inside one dataset (e.g. patient level vs. patient × cluster level) and switch between them.

You should not and you can not change those slots directly. Being a self-describing container is a harsh life, full of dangers and unwanted adventures, and it requires a constant re-evaluation of what goes there and why. `immundata` functions do that internally.

Every transformation returns a new `ImmunData`, so you can stash (not trash) intermediate versions on disk and reproduce any branch of the analysis.

Example:

```r
library(immundata)
      
inp_files <- paste0(system.file("extdata/single_cell", "", package = "immundata"), "/*.csv.gz")
md_file <- system.file("extdata/single_cell", "metadata.tsv", package = "immundata")
md_table <- read_metadata(md_file)
cells_file <- system.file("extdata/single_cell", "cells.tsv.gz", package = "immundata")
cells <- readr::read_tsv(cells_file)

schema <- make_receptor_schema(features = c("cdr3", "v_call"), chains = c("TRB"))

idata <- read_repertoires(path = inp_files, schema = schema, metadata = md_table, barcode_col = "barcode", locus_col = "locus", umi_col = "umis", preprocess = make_default_preprocessing("10x"), repertoire_schema = "Tissue")

print(idata)
```

Printed ImmunData `idata`:

```
── ImmunData ───────────────────────────────────────────────────────────────────────────────────────────

── Receptors: ──

# A duckplyr data frame: 3 variables
   imd_receptor_id cdr3               v_call  
             <int> <chr>              <chr>   
 1            2514 CASSVHPQYF         TRBV2   
 2            7687 CAWSGQGWGGSTDTQYF  TRBV30  
 3            2515 CASSPRPGSTGELFF    TRBV18  
 4            5111 CASSQGLGVSYEQYF    TRBV4-1 
 5            7688 CASSHGQGRTGELFF    TRBV7-2 
 6            7689 CASGLRRGRDSGANVLTF TRBV19  
 7            2516 CSAHRGLGNQPQHF     TRBV20-1
 8            5112 CASSPQGVSNQPQHF    TRBV7-2 
 9               1 CASSSVSGNSPLHF     TRBV7-9 
10            5113 CASPLGALTDTQYF     TRBV2   
# ℹ more rows
# ℹ Use `print(n = ...)` to see more rows

── Annotations: ──

# A duckplyr data frame: 23 variables
   barcode   locus v_call d_call j_call c_gene productive cdr3  cdr3_nt reads  umis filename imd_barcode
   <chr>     <chr> <chr>  <chr>  <chr>  <chr>  <chr>      <chr> <chr>   <dbl> <dbl> <chr>    <chr>      
 1 AAACCTGA… TRB   TRBV2  None   TRBJ2… TRBC2  True       CASS… TGTGCC… 13736    11 /Users/… LB6_AAACCT…
 2 AAACCTGC… TRB   TRBV30 TRBD1  TRBJ2… TRBC2  True       CAWS… TGTGCC…  4062     5 /Users/… LB6_AAACCT…
 3 AAACCTGC… TRB   TRBV18 TRBD2  TRBJ2… TRBC2  True       CASS… TGTGCC…  8617    10 /Users/… LB6_AAACCT…
 4 AAACCTGG… TRB   TRBV4… TRBD1  TRBJ2… TRBC2  True       CASS… TGCGCC…  6811     5 /Users/… LB6_AAACCT…
 5 AAACCTGG… TRB   TRBV7… TRBD1  TRBJ2… TRBC2  True       CASS… TGTGCC… 16836    15 /Users/… LB6_AAACCT…
 6 AAACCTGT… TRB   TRBV19 TRBD2  TRBJ2… TRBC2  True       CASG… TGTGCC…  8805     9 /Users/… LB6_AAACCT…
 7 AAACCTGT… TRB   TRBV2… TRBD1  TRBJ1… TRBC1  True       CSAH… TGCAGT…  8311     6 /Users/… LB6_AAACCT…
 8 AAACGGGA… TRB   TRBV7… TRBD1  TRBJ1… TRBC1  True       CASS… TGTGCC…  3390     3 /Users/… LB6_AAACGG…
 9 AAACGGGA… TRB   TRBV7… TRBD2  TRBJ1… TRBC1  True       CASS… TGTGCC…  4956     4 /Users/… LB6_AAACGG…
10 AAACGGGA… TRB   TRBV2  TRBD1  TRBJ2… TRBC2  True       CASP… TGTGCC…  5625     4 /Users/… LB6_AAACGG…
# ℹ more rows
# ℹ 10 more variables: imd_chain_id <int>, imd_receptor_id <int>, imd_n_chains <dbl>, File <chr>,
#   Tissue <chr>, Prefix <chr>, imd_count <dbl>, imd_repertoire_id <int>, imd_proportion <dbl>,
#   n_repertoires <int>
# ℹ Use `print(n = ...)` to see more rows

── Receptor schema: ──

features:
→ cdr3
→ v_call
chains:
→ TRB


── Repertoire schema: ──

→ Tissue
                     

── List of repertoires: ──

# A tibble: 3 × 4
  imd_repertoire_id Tissue n_barcodes n_receptors
              <int> <chr>       <dbl>       <int>
1                 1 Blood        4085        3976
2                 2 Normal       6797        2950
3                 3 Tumor        7832        3962
```

---

## 🛠 Transformation

Before running the code in the following subsections, execute the code below. Mind that for the example purposes, the data uses the TRB locus only. Change the input receptor schema and the column names to adapt it to the paired-chain case.

```r
library(immundata)
      
inp_files <- paste0(system.file("extdata/single_cell", "", package = "immundata"), "/*.csv.gz")
md_file <- system.file("extdata/single_cell", "metadata.tsv", package = "immundata")
md_table <- read_metadata(md_file)
cells_file <- system.file("extdata/single_cell", "cells.tsv.gz", package = "immundata")
cells <- readr::read_tsv(cells_file)

schema <- make_receptor_schema(features = c("cdr3", "v_call"), chains = c("TRB"))

idata <- read_repertoires(path = inp_files, schema = schema, metadata = md_table, barcode_col = "barcode", locus_col = "locus", umi_col = "umis", preprocess = make_default_preprocessing("10x"), repertoire_schema = "Tissue")
```

### Filter

The key functions for filtering are `filter()` (`dplyr`-compatible) and `filter_immundata()`, which are the same function with a slightly different arguments due to the necessity to comply with `dplyr` interface. Repertoires are reaggregated automatically. Functions `filter_receptors()` and `filter_barcodes()` are used for filter receptor and barcode identifiers, correspondingly.

To filter data, you simply pass predicates like in `dplyr`. Optionally, you can pass `seq_options` that allow you to filter by exact sequence match, regex pattern, or sequence distances using hamming or edit/levenshtein distances. You can pass multiple patterns via `patterns = c("pattern_1", "pattern_2")`.

  1. **Filter by any annotation**
  
      ```r
      idata |> filter(v_call == "TRBV2")
      
      idata |> filter(Tissue == "Blood")
      ```
      
  2. **Chain filters together**
  
      ```r
      # this expression:
      idata |> filter(v_call == "TRBV2", imd_proportion >= 0.0002)
      
      # is the same as this one:
      idata |> filter(v_call == "TRBV2") |> filter(imd_proportion >= 0.0002)
      ```
      
  3. **Filter by sequence distance**
  
      ```r
      idata |> filter(seq_options = make_seq_options(patterns = "CASSELAGYRGEQYF", query_col = "cdr3", method = "lev", max_dist = 3))
      
      idata |> filter(v_call == "TRBV2", seq_options = make_seq_options(patterns = "CASSELAGYRGEQYF", query_col = "cdr3", method = "lev", max_dist = 3))
      ```
      
  4. **Filter by receptor identifiers**
  
      ```r
      idata |> filter_receptors(c(1,2,3))
      ```
      
  5. **Filter by barcodes**
  
      ```r
      target_bc <- cells$barcode[1:3]
      idata |> filter_barcodes(target_bc)
      ```
      
  6. **Filter by repertoire**
  
      ```r
      idata |> filter(imd_repertoire_id == 1)
      
      idata |> filter(Tissue %in% c("Blood", "Tumor"))
      ```

### Annotate

The key function for annotations are `annotate` and `annotate_immundata`. Functions `annotate_receptors()`, `annotate_barcodes()` and `annotate_chains()` are light-weight wrappers around `annotate_immundata()`.
      
  1. **Annotate by any column**
  
      ```r
      idata2 <- annotate(idata = idata, annotations = cells[c("barcode", "ident")], by = c(imd_barcode = "barcode"), keep_repertoires = FALSE)
      idata2 <- idata2 |> filter(!is.na(ident))
      idata2 <- idata2 |> agg_repertoires(schema = "ident")
      
      print(idata2)
      ```
      
  2. **Annotate by receptor identifiers**
  
      ```r
      idata2 <- annotate_receptors(idata = idata, annotations = tibble::tibble(receptor = c(1,2,3), important_data = c("A", "B", "C")), annot_col = "receptor")
      
      idata2 |> filter(important_data %in% c("A", "B"))
      ```
      
  3. **Annotate by barcodes**
  
      ```r
      idata2 <- annotate_barcodes(idata = idata, annotations = cells[c("barcode", "ident")],  annot_col = "barcode", keep_repertoires = FALSE)
      idata2 <- idata2 |> filter(!is.na(ident))
      idata2 <- idata2 |> agg_repertoires(schema = "ident")
      
      print(idata2)
      ```

### Modify

The key functions for this are `mutate` (`dplyr`-compatible) / `mutate_immundata` and the functions from the downstream analysis tools.

  1. **Add or transform one or several annotation columns**
  
      ```r
      idata |> mutate(new_column = "value")
      
      idata |> mutate(big_chains = umis >= 10)
      
      # You can use duckdb functions via `dd$<function>`
      idata |> mutate(dist_to_pattern = dd$levenshtein(cdr3, "CASSSVSGNSPLHF"))
      ```
      
  2. **Add columns with sequence distance to patterns**
  
      ```r
      patterns <- c("CASSVHPQYF", "CAWSGQGWGGSTDTQYF", "CASSPRPGSTGELFF")
      idata |> mutate(seq_options = make_seq_options(query_col = "cdr3", patterns = patterns, method = "lev"))
      
      idata |> mutate(seq_options = make_seq_options(query_col = "cdr3", patterns = patterns, method = "lev", name_type = "pattern"))
      ```
      
  3. **Modify a subset of column values**
  
      ```r
      idata |> mutate(found_pattern = if_else(cdr3 == "CASSVHPQYF", 1, 0))
      ```
---

## 📈 Analysis

### Basic analysis using `immundata`

```r
# Find receptors from several repertoires which have >60 abundance
# and get their barcodes
idata2 <- idata |> filter(imd_count >= 60)
target_chains <- idata2$annotations |> select(imd_barcode, imd_count, cdr3, v_call, Tissue) |> collect()
target_chains

# You can then annotate those receptors in your single-cell data using
# the `imd_barcode` column as a key.
```

### Advanced analysis using `immundata`

```r
# Install the latest pre-1.0 version of immunarch
# pak::pkg_install(immunomind/immunarch)

library(immunarch)

ov_heatmap <- airr_public_index(idata, "jaccard")
pheatmap::pheatmap(ov_heatmap)

clonal_space_homeo <- airr_clonality(idata, "prop")
ggplot2::ggplot(data = clonal_space_homeo) + geom_col(aes(x = Tissue, y = occupied_prop, fill = clonal_prop_bin)) + ggplot2::theme_bw()
```

---

## 🧩 Use Cases

> [!TIP]
> Tutorial on `immundata` + `immunarch` is available [on the ecosystem website](https://immunomind.github.io/docs/tutorials/single-cell/).
>
> Read the previous section about the analysis.
>
> This section is still under construction.

### Bulk -- RepSeq, AIRRSeq

> [!CAUTION]
> 🚧 Under construction. 🚧

Pretty much the whole README is either about single-chain or paired-chain data.

Consider passing `make_default_preprocessing("airr")` or `make_default_preprocessing("10x")` to `read_repertoires(..., preproc = <here>, ...)` to have convenient processing of the corresponding formats.

Pass `count_col` to `read_repertoires(..., count_col = "Counts", ...)` to assign counts to chains.

### Single-cell and paired-chain -- scRNAseq, scVDJseq, scTCRseq, scBCRseq

> [!CAUTION]
> 🚧 Under construction. 🚧

Make sure to pass `make_default_preprocessing("10x")`, `locus_col` and `barcode_col` to read paired-chain data. Drop the `locus_col` if you want to read single-chain data only.

```r
library(Seurat) 

sdata <- ...  # Load the Seurat data
idata <- ...  # Load the corresponding AIRR data

# Get both GEX and metadata
smeta <- data.frame(
  barcode      = colnames(sdata),
  cluster      = Idents(sdata),
  gene_MS4A1   = FetchData(sdata, "MS4A1")[,1]
)

idata <- annotate_barcodes(idata, smeta, annot_col = "barcode")
```

```r
library(AnnDataR)

adata <- ...  # Load the AnndataR data
idata <- ...  # Load the corresponding AIRR data

ameta <- data.frame(
  barcode  = adata$obs_names,
  cluster  = adata$obs$cell_type,
  gene_GCG = adata$X[, "GCG"]
)

idata <- annotate_barcodes(idata, ameta, annot_col = "barcode")
```

---

## 🧠 Advanced Topics 

### Developers

#### Integrate into your package 

> [!CAUTION]
> 🚧 Under construction. 🚧

`immundata` is created in mind with the mission of replacing typical data frame-based formats, usually not following the AIRR-C file standard.

#### Extend with functions

> [!CAUTION]
> 🚧 Under construction. 🚧

S3 methods etc.

### How `immundata` reads the data

By design, **`immundata`** data-loading pipeline is **three** steps, rather than one giant function. This promotes modularity, easier debugging, and flexible usage:

1.  **(Optionally) Load the metadata** via `read_metadata()`.
    -   This ensures your metadata has the correct file paths, absolute or relative.
2.  **Load the repertoire files** from disk via `read_repertoires()`.
    -   This function unifies your data (be it 1 file or 100 files) and **outputs** a Parquet file:
        -   **`annotations.parquet`** (cell-level data, sample metadata, etc.)
    -   It then calls `read_immundata()` to return a fully instantiated `ImmunData` object that uses the newly created files on the disk as a source. The helps two-fold: you don't lose your data, and it allows `immundata` to run an optimized code when necessary.
3.  **(Optionally) Load the same `ImmunData` files later** with `read_immundata()`.
    -   If you need to reopen the data in a future R session, you don't have to redo the entire pipeline.
    -   Just call `read_immundata(path_to_immundata_folder)` where the folder contains `annotations.parquet`.
    -   With this approach, you **never** need to re-parse your raw AIRR files once you've generated the Parquet-based `immundata` format.

Why split it up?

-   **Modularity**: If something breaks, you can debug whether it's in metadata parsing or the actual repertoire table creation.
-   **Reusability**: It is straightforward to share one folder with two `immundata` files.
-   **Performance**: Once your data is in `immundata` format, you can load it in future sessions in **constant time** without merging or parsing again.

### Custom functions for analysis

> [!CAUTION]
> 🚧 Under construction. 🚧

1) Function is supported by `duckdb` - then use `dd$<function_name>`

2) Use SQL

3) Run a completely custom function

### Change RAM limits to accelerate the backend computations 

You can change the [RAM limits in duckplyr](https://duckplyr.tidyverse.org/articles/large.html#memory-usage).

### Make `immundata` even faster with data engineering tricks

You can use [hive partitioning](https://duckdb.org/docs/stable/data/partitioning/hive_partitioning.html) to accelerate analysis. Recommended columns are locus, V gene segment, and sequence length.

### Save your intermediate data for faster computations and reproducibility 

Consider caching your data to disk after heavy operations, such as distance computations.

---

## 🏷 About 

### Citation 

*... coming soon ...*

### License 

The package is freely distributed under the Apache-2.0 license. You can read more about it [here](https://www.tldrlegal.com/license/apache-license-2-0-apache-2-0).

### Author and contributors 

**Vadim I. Nazarov – main author and developer**

Vasily Tsvetkov

*... more to come ...*

### Commercial usage 

`immundata` is free to use for commercial usage as per Apache-2.0 license. However, corporate users will not get a prioritized support for `immundata`- or AIRR-related issues. The priority of open-source tool `immundata` is open-source science.

If you are looking for prioritized support and setting up your data pipelines, consider contacting [Vadim Nazarov](https://www.linkedin.com/in/vdnaz/) for commercial consulting / support options / workshops and training sessions / designing data platforms and machine learning systems for multi-omics / or anything related.

---

## 🤔 FAQ

1.  **Q: Why all the function names or ImmunData fields are so long? I want to write `idata$rec` instead of `idata$receptors`.**

    A: Two major reasons – improving the code readability and motivation to leverage the autocomplete tools. Please consider using `tab` for leveraging autocomplete. It accelerates things x10-20.

2.  **Q: How does `immundata` works under the hood, in simpler terms?**

    A: Picture a three-layer sandwich:
    
    - `Arrow` files on disk hold the raw tables in column-compressed Parquet.

    - `DuckDB` is an in-process SQL engine that can query those files without loading them fully into RAM.

    - `duckplyr` glues `dplyr` verbs (filter, mutate, summarise, …) to DuckDB SQL, so your R code looks exactly like a tidyverse pipeline while the heavy lifting happens in C++.

    When you call `read_repertoires()`, immundata writes `Arrow` parts, registers them with `DuckDB`, and returns a `duckplyr` table. Every later verb is lazily translated into SQL; nothing is materialised until a step truly needs physical data (e.g. a plot or an algorithm that exists only in R).

    References
    1. [Arrow for R – columnar file format](https://arrow.apache.org/docs/r/)
    2. [DuckDB – embedded analytical database](https://duckdb.org/)
    3. [duckplyr – API/implementation details](https://duckplyr.tidyverse.org/index.html)

3.  **Q: Why do you need to create Parquet files with receptors and annotations?**

    A: Those are intermediate files, optimized for future data operations, and working with them significantly accelerates `immundata`. I will post a benchmark soon.

4.  **Q: Why does `immundata` support only the AIRR standard?!**

    A: The short answer is because a single, stable schema beats a zoo of drifting ones.
    
    The practical answer is that `immundata` allows some optionality – you can provide column names for barcodes, etc.
    
    The long answer is that the amount of investments required not only for the development, but also for the continued support of parsers for different formats, is astonishing. I developed parsers for 10+ formats for `tcR` / `immunarch` packages. I would much prefer for upstream tool developers not to change their format each minor version, breaking pretty much all downstream pipelines and causing all sorts of pain to end users and tools developers – mind you, without bearing a responsibility to at least notify, but ideally fix the broken formats they introduced. The time of the Wild West is over. The AIRR community did an outstanding job creating its standard. Please urge the creators of your favourite tools or fellow developers to use this format or a superset, like `immundata` does.

    `immundata` does not and will not explicitly support other formats. This is both a practical stance and communication of crucial values, put into `immundata` as part of a broader ecosystem of AIRR tools. The domain is already too complex, and we need to work together to make this complexity manageable. A healthy ecosystem is not the same as a complex ecosystem.

5.  **Q: Why is it so complex? Why do we need to use `dplyr` instead of plain R?**

    A: The short answer is:

    -   faster computations;
    -   code, that is easy to maintain and support by other humans;
    -   better data skills thanks to thinking in immutable transformations,
    -   in most cases you don't really need complex transformations, so we can optimize 95% of all AIRR data operations behind the scenes.

6.  **Q: How do I use `dplyr` operations that `duckplyr` doesn't support yet?**

    A: Let's consider several use cases.

    **Case 0.** You are missing `group_by` from `dplyr`. Use `summarise(.by = ???, ...)`.

    **Case 1.** Your data can fit into RAM. Call `collect` and use `dplyr`.

    **Case 2.** Your data won't fit into RAM but you must run a heavy operation on all rows. You can rewrite functions in SQL. You can break it into supported pieces (e.g. pre-filter, pre-aggregate) that DuckDB can stream, write an intermediate Parquet with `compute_parquet()`, then loop over chunks, collect them, and run the analysis.

    **Case 3.** Your data won't fit into RAM, but before running intensive computations, you are open to working with smaller dataset first. Filter down via `slice_head(n=...)`, iterate until the code works, then run the same pipeline on the full dataset.

7.  **Q: You filter out non-productive receptors. How do I explore them?**

    A: Do not filter out non-productive receptors in `preprocess` in `read_repertoires()`.

8.  **Q: Why does `immundata` have its own column names for receptors and repertoires? Could you just use the AIRR format - repertoire_id etc.?**

    A: The power of `immundata` lies in the fast re-aggregation of the data, that allows to work with whatever you define as a repertoire on the fly via `agg_repertoires`. Hence I use a superset of the AIRR format, which is totally acceptable as per their documentation.

9.  **Q: What do I do with following error: "Error in `compute_parquet()` at [...]: ! ?***

    A: It means that your repertoire files have different schemas, i.e., different column names. You have two options.

    **Option 1:** Check the data and fix the schema. Explore the reason why the data have different schemas. Remove wrong files. Change column names. And try again.

    **Option 2:** If you know what you are doing, pass argument `enforce_schema = FALSE` to `read_repertoires`. The resultant table will have NAs in the place of missing values. But don't use it without considering the first option. Broken schema usually means that there are some issues in the how the data were processed upstream.

10. **Q: `immundata` is too verbose, I'm tired of all the messages. How to turn them off?**
    
    A: Run the following code `options(rlib_message_verbosity = "quiet")` in the beginning of your R session to turn off messages.

11. **Q: I don't want to use `pak`, how can I use the good old `install.packages` or `devtools`?**

    A: Nothing will stop you, eh? You are welcome, but I'm not responsible if something won't work due to issues with dependencies:

    ```r
    # CRAN release
    install.packages("immundata")

    # GitHub release
    install.packages(c("devtools", "pkgload"))
    devtools::install_github("immunomind/immundata")
    devtools::reload(pkgload::inst("immundata"))

    # Development version
    devtools::install_github("immunomind/immundata", ref = "dev")
    devtools::reload(pkgload::inst("immundata"))
    ```

12. **Q: Why are the counts for receptors available only after all the aggregation?**

    A: Counts and proportions are properties of a receptor inside a specific repertoire. A receptor seen in two samples will be counted twice – once per repertoire. Until receptors and repertoires are defined, any "count" would be ambiguous. That's why the numbers appear only after `agg_receptors()` and `agg_repertoires()` have locked those definitions in.
