#' @name bootstrapSample
#' @export bootstrapSample
#' @author Gulzina Kuttubekova
#'
#' @title Generate bootstrap samples
#'
#' @description Takes in dataset in a vector form. Returns a list of n bootstrap sample.
#'
#' @param n number of bootstrap samples to generate
#' @param s sample dataset in a vector form
#'
#' @return A list of n bootstrap samples
#'
#' @example bootstrapSample(rnorm(30), 8)
#'
#' @importFrom plotly plot_ly layout animation_opts animation_slider animation_button
#' @importFrom dplyr %>%

bootstrapSample <- function(s, n) {
  # assert that n is scalar, numeric, not NaN or Inf
  assert( # what to use here? check or expect?????
    check_scalar(n),
    check_int(n, na.ok = FALSE)
  )
  if (is.nan(n) || is.infinite(n)) {
    stop("n can't be NaN or Inf")
  }

  # assert that s is non-empty vector, does not contain Inf, NA or NaN
  assert( # what to use here? check or expect?????
    check_vector(s),
    not_empty(s),
    anyMissing(s),
    anyInfinite(s)
  )

  # constants
  m = length(s)

  # store bootstrap samples in a list
  samples <- list()

  # create n bootstrap samples
  for (i in 1:n) {
    ss <- sample(s, m)
    samples[[i]] <- ss
  }

  # check if result is in the list form and return it
  assert(
    check_list(samples)
  )
  return(samples)
}


