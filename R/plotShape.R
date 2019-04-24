#' @name plotShape
#' @export
#' @author Charlotte Roiger
#'
#' @title Plot a random distribution with differing shape
#'
#' @description Returns a ggplot object of certain shape (right-skewed, left-skewed, symmetric)
#'
#' @param randomDist Randomly generated number to signify type of distribution
#'
#' @return A list consisting of one character string and a ggplot object
#'
#' @import ggplot2
#' @importFrom dplyr case_when
#' @import checkmate

plotShape <- function(randomDist=1){
  assertNumber(randomDist)
  randomShape <- dplyr::case_when(
    randomDist == 1 ~ c("rbeta(10000,2,5)", 'rs'),
    randomDist ==2 ~ c("rbeta(10000,5,2)", 'ls'),
    randomDist ==3 ~ c("rbeta(10000,5,5)", 'sym'))
  dist <- eval(parse(text=randomShape[1]))
  plot.dist <- dist %>%
    data.frame() %>%
    ggplot(aes(x = dist)) +
    geom_histogram(binwidth = 0.1)
  return(list(randomShape[2], plot.dist))
}

