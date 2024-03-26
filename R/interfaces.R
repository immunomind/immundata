SequenceSimilaritySearchInterface <- R6Class(
  "SequenceSimilaritySearchInterface"
)

BasicAnalysisInterface <- R6Class(
  "BasicAnalysisInterface",
  public = list(
    explore = function() {

    },
    clonality = function() {

    },
    overlap = function() {

    },
    gene_usage = function() {

    },
    diversity = function() {

    }
  )
)

PublicClonotypeInterface <- R6Class(
  "PublicRepertoireInterface",
  public = list(
    public_repertoire = function() {

    },
    track_clonotypes = function() {

    },
    search_database = function() {

    }
  )
)
