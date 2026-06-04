#' Collect warnings to the inputwarnings data.frame
#' @export
InputWarnings <- R6::R6Class(
  "InputWarnings",
  public = list(
    warnings = NULL,
    initialize = function(init_df) {
      stopifnot( all(c("code", "warningText") %in% colnames(init_df)))
      self$warnings <- init_df
    },
    add = function(code, nl_text, en_text, National) {
      warningText <- if (National == "Nederlands") nl_text else en_text
      self$warnings[1 + nrow(self$warnings), ] <- c(code, warningText)
      invisible(self)
    }
  )
)
