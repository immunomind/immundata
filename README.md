# immundata-rlang

## Installation

## Quick start

```r
library(immundata)
library(dplyr)
library(duckplyr)
```

## Input / output

### Supported formats

parquet etc.


### Read one or multiple files into `immundata`

Suppose you have several files. How to read them?

#### 1. Pass a singular file name

#### 2. Pass a vector of file names

#### 3. Pass a glob of files


### Working with the repertoire metadata file

`immundata` modularizes different parts to make sure ??? (modularity / one big function is bad) Henceforth, `immundata` splits the repertoire dataset loading into three steps:

  1. Optionally, load the metadata via `load_metadata`
  
  2. Load the repertoire files from the disk via `load_repertoires` and convert them into `immundata` files.
  
  3. Load the ImmunData files from the converted files via `load_immundata` as the final step of `load_repertoires`.
  
After converting the files to the `immundata` format, you can load them directly with `load_immundata`.


### Re-aggregating repertoires using receptor and repertoire schemas

This is the key concept that distinguished `immundata` from DataFrame-based libraries.

- people analyse a specific receptors
- data lineage is crucial for full reproducibility


### Modalities - bulk, single-cell, spatial, hybrid

#### Bulk data (RepSeq, AIRRSeq)

#### Single-cell data (scRNAseq, scVDJseq, scTCRseq, scBCRseq)

- load annotation data
- do something
- write the annotation data back
- visualize AIRR with annotations data
- visualize SC with annotation data

#### Spatial data

- load annotation data
- do something
- write the annotation data back
- visualize AIRR with annotations data
- visualize SC with annotation data

#### Hybrid data

...

#### Multi-locus data

...


### Preprocessing strategies

- filtering non productive
- double contigs
- double BCR chains
- locus


## Data manipulation

### Filtering


### Analyse the data

#### Immunarch


## Advanced topics

### Integrate into your package

Take a look at `immunarch`.

### Change RAM limits to accelerate the backend computations

...

### Caching strategies

...


## About

### Citation

### License

### Author and contributors

### Commercial usage

`immundata` is free to use for commercial usage.
However, corporate users will not get a prioritized support for `immundata`-related issues, 
immune repertoire analysis questions or data engineering questions, related to building
scalable immune repertoire and other -omics pipelines.
The priority of open-source tool `immundata` is open-source science.

If you are looking for prioritized support and setting up your data pipelines, consider contacting Vadim Nazarov for commercial consulting and support options.

## FAQ

1. **Q: Why all the function names or ImmunData fields are so long? I want to write `imdata$rec` instead of `imdata$receptors`.**
   
   A: Two major reasons - improving the code readability and motivation to leverage the autocomplete tools.

2. **Q: How does `immundata` works under the hood, in simpler terms?**
   
   A: `immundata` uses the fantastic `duckplyr` package
   
   References:
   
   - [duckplyr](https://duckplyr.tidyverse.org/index.html)

3. **Q: Why do you need to create Parquet files with receptors and annotations?**
   
   A: First of all, you can turn it off. Second, those are intermediate files, optimized for future
data operations, and working with them significantly accelerates `immundata`.
Take a look at our benchmark page to learn more: `link`

4. **Q: Why does `immundata` support only the AIRR standard?!**

   A: Because standards, but `immundata` allows some level of optionality - you can provide column names for barcodes, etc.

5. **Q: Why is it so complex? Why do we need to use `dplyr` instead of plain R?**

   A: The short answer is:
     - faster computations,
     - code, that is easy to maintain and support by other humans,
     - and better data skills.
   
   For the long answer, let me give you more details on each of the bullepoint.

6. **Q: How do I get to use all operations from `dplyr`? `duckplyr` doesn't support some operations, which I need.**
   
   A: Let's consider several use cases.

   **Case 0.** You are missing `group_by` from `dplyr`.

   **Case 1.** Your data can fit into RAM.

   **Case 2.** Your data won't fit into RAM, and you really need to work on all of this data.

   **Case 3.** Your data won't fit into RAM, but before running intensive computations, you are open to working with smaller dataset first.

7. **Q: You filter out non-productive receptors. How do I explore them?**

   A: option for saving non-productive chains to a separate file

8. **Q: Why does `immundata` have its own column names for receptors and repertoires? Could you just use the AIRR format - repertoire_id etc.?**

   A: The power of `immundata` lies in the fast re-aggregation of the data, that allows to work with whatever you define as a repertoire on the fly via `ImmunData$build_repertoires(schema = ...)`
