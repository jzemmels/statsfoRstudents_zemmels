#' @name randomSample
#' @export
#' @author Joe Zemmels
#'
#' @title Generate an arbitrary number of random samples from a normal distribution
#'
#' @description Returns a data frame whose columns are random samples from a normal distribution
#'
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#' @param sampleSize size of each sample
#' @param numSamples number of random samples to be generated
#'
#' @return A data frame whose columns are random samples from a normal distribution
#'
#' @examples
#' head(randomSample(mu=1,sigma=4,sampleSize=100,numSamples=5))
#'
#' @import magrittr
#' @import checkmate
#' @importFrom stats rnorm

randomSample <- function(mu=0,sigma=1,sampleSize=25,numSamples=1){
  assertNumeric(c(mu,sigma,sampleSize,numSamples),any.missing = FALSE,finite = TRUE)

  dat <- rnorm(n=sampleSize*numSamples,mean=mu,sd=sigma) %>%
    matrix(ncol=numSamples) %>%
    data.frame()

  assertDataFrame(dat,any.missing = FALSE)
  return(dat)
}

#' @name randomSample_histogram
#' @export
#' @author Joe Zemmels
#'
#' @title Generate a histogram of a random sample
#'
#' @description Returns a ggplot or plotly object of a histogram generated from a random sample
#'
#' @param sampleData data frame of random samples from a normal distribution
#' @param binwidth width of bins in histogram
#' @param variableName name of variable to be labeled on the x-axis of histogram
#' @param plotly specifies whether a plotly object is desired
#'
#' @return A ggplot or plotly object, depending on whether the plotly argument was specified TRUE or FALSE.
#'
#' @examples
#' dat <- randomSample(mu=1,sigma=4,sampleSize=100,numSamples=5)
#' randomSample_histogram(dat)
#'
#' @importFrom plotly ggplotly
#' @import ggplot2
#' @import checkmate
#' @importFrom rlang .data

randomSample_histogram <- function(sampleData,binwidth=1,variableName="Height",plotly=FALSE){
  assertDataFrame(sampleData,any.missing = FALSE)
  assertNumber(binwidth,na.ok = FALSE,finite = TRUE)
  assertCharacter(variableName,any.missing = FALSE)
  assertLogical(plotly)

  dat <- data.frame(x=sampleData[,ncol(sampleData)]) #grabs last sample in sampleData

  #sampleBinwidth <- as.numeric(2*(quantile(dat$x,.75) - quantile(dat$x,.25))*(length(dat$x))^(-1/3)) ##https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule

  plt <- dat %>%
    ggplot(aes(x=.data$x)) +
    geom_histogram(binwidth=binwidth,fill="darkgreen") +
    xlab(variableName)

  if(plotly==TRUE){
    plt <- ggplotly(plt)
  }

  return(plt)
}

#' @name updateSampleMeans
#' @export
#' @author Joe Zemmels
#'
#' @title Create/update a data frame of sample means
#'
#' @description Creates or updates a data frame of sample means from a randomSample object passed to it
#'
#' @param sampleData data frame of random samples from a normal distribution
#' @param sampleMeans data frame of sample means (empty data frame is initialized if this is argument is blank)
#'
#' @return A data frame of sample means from each sample in randomSample
#'
#' @examples
#' dat <- randomSample(numSamples=100)
#' head(updateSampleMeans(sampleData=dat))
#'
#' @import checkmate

updateSampleMeans <- function(sampleData,sampleMeans=NULL){
  assertDataFrame(sampleMeans,null.ok = TRUE)
  assertDataFrame(sampleData,null.ok = TRUE,any.missing = FALSE)

  if(is.null(sampleMeans)){
    sampleMeans <- data.frame() #initialize an empty data frame if sampleMeans hasn't been passed
  }

  sampleMeans <- data.frame(means = c(sampleMeans$means,colMeans(sampleData)))

  return(sampleMeans)
}

#' @name sampleMeans_histogram
#' @export
#' @author Joe Zemmels
#'
#' @title Generate a histogram of sample means
#'
#' @description Returns a ggplot or plotly object of a histogram generated from a set of sample means
#'
#' @param sampleMeans data frame of sample means
#' @param binwidth width of bins in histogram
#' @param variableName name of variable to be labeled on the x-axis of histogram
#' @param plotly specifies whether a plotly object is desired
#'
#' @return A ggplot or plotly object, depending on whether the plotly argument was specified TRUE or FALSE.
#'
#' @examples
#' dat <- randomSample(numSamples=100)
#' sampleMeans <- updateSampleMeans(sampleData=dat)
#' sampleMeans_histogram(sampleMeans,binwidth=.1)
#'
#' @importFrom plotly ggplotly
#' @import ggplot2
#' @import magrittr
#' @import checkmate
#' @importFrom rlang .data

sampleMeans_histogram <- function(sampleMeans,binwidth=1,variableName="Height",plotly=FALSE){
  assertDataFrame(sampleMeans,any.missing = FALSE)
  assertNumber(binwidth,na.ok = FALSE,finite = TRUE)
  assertCharacter(variableName)
  assertLogical(plotly)

  #meansBinwidth <- as.numeric(2*(quantile(sampleMeans$means,.75) - quantile(sampleMeans$means,.25))*(length(sampleMeans$means))^(-1/3)) #https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule

  plt <- sampleMeans %>%
    ggplot(aes(x=.data$means)) +
    geom_histogram(binwidth=binwidth,fill="darkgreen",na.rm=TRUE) +
    xlab(paste("Mean",variableName))

  if(plotly==TRUE){
    plt <- ggplotly(plt)
  }

  return(plt)
}
