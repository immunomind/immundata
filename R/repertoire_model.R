RepertoireModel <- R6Class(
  "RepertoireModel",
  public = list(
    by = "",
    names = "",
    initialize = function(.by, .names) {
      ###
      # Argument type checks
      ###
      assert_character(.by, min.chars = 1, min.len = 1)
      assert(
        check_character(.names, min.chars = 1, min.len = 1),
        check_function(.names)
      )

      ###
      # Body
      ###
      self$by <- .by
      self$names <- .names # TODO: when do we call the function to generate repertoire names?
    },
    print = function() {

    }
  )
)
