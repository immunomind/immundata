#' @importFrom R6 R6Class
ImmunData <- R6Class(
  "ImmunData",
  public = list(
    data = NULL,

    initialize = function(data, engine) {
      self$data <- data
    },

    filter = function(...) {
      self$data <- filter(self$data, ...)
      return(self)
    },

    select = function(...) {
      self$data <- select(self$data, ...)
      return(self)
    },

    group_by = function(...) {

    },

    summarize = function(...) {
      self$data <- summarise(self$data, ...)
      return(self)
    },

    show = function() {
      return(self$data)
    }
  )
)

