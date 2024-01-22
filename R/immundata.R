#' @importFrom R6 R6Class
ImmunData <- R6Class(
  "ImmunData",
  public = list(
    data = NULL,

    initialize = function(data) {
      self$data <- data
    },

    filter = function(...) {
      self$data <- dplyr::filter(self$data, ...)
      return(self)
    },

    select = function(...) {
      self$data <- dplyr::select(self$data, ...)
      return(self)
    },

    summarize = function(...) {
      self$data <- dplyr::summarise(self$data, ...)
      return(self)
    },

    show = function() {
      return(self$data)
    }
  )
)

