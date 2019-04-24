#' @name plotOutlier
#' @export
#' @author Charlotte Roiger
#'
#' @title Plot a random distribution with differing outliers
#'
#' @description Returns a ggplot object of a certain type of outlier (none, one, multiple)
#'
#' @param randomDist Randomly generated number to signify type of distribution
#'
#' @return A list consisting of one character string and a ggplot object
#'
#' @import ggplot2
#' @importFrom dplyr case_when
#' @import checkmate

plotOutlier <- function(randomDist=1){
  assertNumber(randomDist)
  randomOutlier <- dplyr::case_when(
    randomDist == 1 ~ c('rnorm(10000)', "none", '.025'),
    randomDist ==2 ~ c("c(rbeta(10000,2,5), rep(0.95,1000))", 'one', '.1'),
    randomDist ==3 ~ c("c(rbeta(10000,10,2), rep(0.1,1000), rep(0.3,1000))", 'multiple', '.1'))
  dist <- eval(parse(text=randomOutlier[1]))
  plot.dist <- dist %>%
    data.frame() %>%
    ggplot(aes(x = dist)) +
    geom_histogram(binwidth = 0.1)
  return(list(randomOutlier[2], plot.dist))
}

