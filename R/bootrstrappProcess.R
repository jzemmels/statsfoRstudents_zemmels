#' @name bootstrapProcess
#' @export bootstrapProcess
#' @author Gulzina Kuttubekova
#'
#' @title Show animated/still bootstapping process
#'
#' @description Takes in dataset in a vector form. If anime = TRUE, returns animated time series plot which shows elements sampled with replacement, otherwise returns still plot of bootstrap sample.
#'
#' @param s sample dataset in a vector form
#' @param anime boolean, where TRUE = T = 1, FALSE = F = 0
#'
#' @return A time series cumulative animation.
#'
#' @examples bootstrapProcess(rnorm(30), TRUE)
#'
#' @importFrom plotly plot_ly layout animation_opts animation_slider layout add_text
#' @importFrom dplyr %>%

bootstrapProcess <- function(s, anime) {
  ### here check the inputs and all other things!!!
  # assert that s is non-empty vector, does not contain Inf, NA or NaN
  assert( # what to use here? check or expect?????
    check_vector(s),
    not_empty(s),
    anyMissing(s),
    anyInfinite(s)
  )

  # check if correct valid boolean value is entered
  v <- c("TRUE", "T", 1, "FALSE", "F", 0)
  if (!anime %in% v) {
    stop("Enter a valid boolean anime value!")
  }

  # constants
  n = length(s)

  # store the bootstrap sample with time ids
  df <- data.frame(x = NA, draw = NA)

  # create bootstrap sample and record each step
  for (i in 1:n) {
    rn <- sample(1:n, 1)
    df[i,] <- c(s[rn], i)
  }

  if (anime == "TRUE" || anime == 1 || anime == "T") {
    # plot cumulatiove animation
    plot <- df %>%
      plot_ly(
        x = ~draw,
        y = ~x,
        frame = ~draw,
        #text = ,
        type = 'scatter',
        mode = 'markers'
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "Time ", font = list(color="red"))
      ) %>%
      animation_opts(
        frame = 2000,
        transition = 0,
        easing = "elastic",
        redraw = FALSE
      ) %>%
      layout(
        title = "Sampling with Replacement"
      )
    plot

    } else if (anime == "FALSE" || anime == 0 || anime == "F") {
    # set font style for text
    t <- list(
      family = "sans serif",
      size = 13,
      color = toRGB("grey50"))

    # plot bootstrap sample
    plot <- df %>%
      plot_ly(
        x = ~draw,
        y = ~x,
        text = ~x,
        type = 'scatter',
        mode = 'markers'
      ) %>%
      layout(
        title = "Sampling with Replacement"
      ) %>%
      add_text(
        textfont = t,
        textposition = "top right"
      )
    plot
  }
}





