# immundata-rlang

## Installation

## Quick start

```r
library(immundata)
```

## Input / output

### Supported formats

parquet etc.

### Read one or multiple files into `immundata`

Suppoose you have several files. How to read them?

#### 1. Pass a singular file name

#### 2. Pass a vector of file names

#### 3. Pass a glob of files


### Manipulate the data

#### Filtering


### Analyse the data

#### Immunarch


## Advanced

### Integrate into your package

Take a look at `immunarch`.

### Change RAM limits to accelerate the backend computations

...


## FAQ

Q: How does `immundata` works under the hood, in simpler terms?
A:

Q: Why do you need to create Parquet files with receptors and annotations?
A: First of all, you can turn it off. Second, those are intermediate files, optimized for future
data operations, and working with them significantly accelerates `immundata`.
Take a look at our benchmark page to learn more: `link`

Q: Why does `immundata` support only the AIRR standard?!
A: Because standards, but `immundata` allows some level of optionality - you can provide column names for barcodes, etc.

Q: Why is it so complex? Why do we need to use `dplyr` instead of plain R?
A: The short answer is:
 - faster computations,
 - code, that is easy to maintain and support by other humans,
 - and better data skills.
 
 For the long answer, let me give you more details on each of the bullepoint.

Q: How do I get to use all operations from `dplyr`? `duckplyr` doesn't support some operations, which I need.
A: Let's consider several use cases.

Case 0. You are missing `group_by` from `dplyr`.

Case 1. Your data can fit into RAM.

Case 2. Your data won't fit into RAM, and you really need to work on all of this data.

Case 3. Your data won't fit into RAM, but before running intensive computations, you are open to working with smaller dataset first.

Q: You filter out non-productive receptors. How do I explore them?
A: option for saving non-productive chains to a separate file

Q: Why does `immundata` have its own column names for receptors and repertoires?
Could you just use the AIRR format - repertoire_id etc.?
A: The power of `immundata` lies in the fast re-aggregation of the data, that allows to 
work with whatever you define as a repertoire on the fly via `ImmunData$build_repertoires(schema = ...)`
