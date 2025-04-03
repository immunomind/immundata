ClonotypeModel <- R6Class(
  "ClonotypeModel",
  public = list(
    initialize = function(.code = "", .chain = "", .sequence = "", .gene = "") {
      assert_clonotype_info <- function(.chain, .sequence, .gene) {
        assert(
          check_character(.chain, pattern = ""),
          check_character(.sequence, pattern = ""),
          check_character(.gene, pattern = ""),
          combine = "and"
        )
      }

      ###
      # Argument type checks
      ###
      assert(
        check_character(.code),
        check_clonotype_info(.chain, .sequence, .gene)
      )

      if (nchar(.code)) {
        .code <- stringr::str_split(input, "\\+") |> unlist() # Yes, I do like pipes, they improve the readability.
        # .chain
        # .sequence
        # .gene
        assert_clonotype_info(.chain, .sequence, .gene)
      }

      ###
      # Body
      ###
    }
  )
)
