#' Factory for Quantile Functions (with 'higher' method)
#'
#' Creates a function that calculates specified quantiles (using the 'higher' method, type = 2) for a numeric vector.
#'
#' @param props Numeric vector of probabilities (between 0 and 1) at which to compute quantiles.
#' @return A function that takes a numeric vector and returns the quantiles at the specified probabilities.
#' @examples
#' # Create a quantile function for 40% and 80%
#' q_fn <- make_quantile(c(0.4, 0.8))
#' # Use it on a numeric vector
#' x <- c(3, 7, 8, 12, 20)
#' q_fn(x)
#' @export
make_quantile <- function(props) {
  # Return a function that takes a numeric vector and returns the specified quantiles
  function(x) {
    quantile(x, probs = props, type = 2)
  }
}