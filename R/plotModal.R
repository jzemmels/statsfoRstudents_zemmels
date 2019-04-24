#' @name plotModal
#' @export
#' @author Charlotte Roiger
#'
#' @title Plot a random distribution with differing modality
#'
#' @description Returns a ggplot object of certain modality (unimodal, bimodal, uniform)
#'
#' @param randomDist Randomly generated number to signify type of distribution
#'
#' @return A list consisting of one character string and a ggplot object
#'
#' @import ggplot2
#' @importFrom case_when
#' @import checkmate

plotModal <- function(randomDist=1){
  assertNumber(randomDist)
  randomModality <- dplyr::case_when(
    randomDist == 1 ~ c('runif(1,0,1)', "unif", '.025'),
    randomDist ==2 ~ c("rbeta(10000,2,5)", 'unimod', '.1'),
    randomDist ==3 ~ c("c(rbeta(5000,10,2), rbeta(5000,1,5))", 'bimod', '.1'))
    dist <- eval(parse(text=randomModality[1]))
    plot.dist <- dist %>%
      data.frame() %>%
      ggplot(aes(x = dist)) +
      geom_histogram(binwidth = as.numeric(randomModality[3]))
    return(list(randomModality[2], plot.dist))
}

