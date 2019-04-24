#' @name Errors
#' @export
#' @author Eryn Blagg
#'
#' @title Plot Type I and Type II Error
#'
#' @description Returns a ggplot or plotly object of a normal distribution with a specified significance level
#'
#' @param means mean of normal distribution
#' @param sds standard deviation of normal distribution
#' @param alpha significance level
#' @param direction direction of alternative hypothesis
#' @param plotly specifies whether a plotly object is desired
#'
#' @return A ggplot or plotly object, depending on whether the plotly argument was specified TRUE or FALSE.
#'
#'
#' @importFrom plotly ggplotly
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom rlang .data

errors <- function(means=0,sds=1,alpha=.05,direction=intToUtf8("8800"),alt= 1 ,plotly=FALSE){

  x <- seq(means - 3*sds,means + 3*sds,by = sds/100)

  initalplt <- ggplot(as.data.frame(x), aes(x)) +
    stat_function(fun = dnorm,args=list(mean=means,sd=sds))

  if(direction == intToUtf8("8800")){ #two-sided.
    pltup <- initalplt +
      stat_function(fun = dnorm,args=list(mean=means,sd=sds),
                    xlim = c(quantile(x,probs = 1-alpha/2),max(x)),
                    geom = "area") +
      stat_function(fun = dnorm,args=list(mean=means,sd=sds),
                    xlim = c(min(x),quantile(x,probs = alpha/2)),
                    geom = "area")
    pltlw<-stat_function(fun = dnorm,args=list(mean=means,sd=sds),
                         xlim = c(quantile(x,probs = alpha/2),quantile(x,probs = 1-alpha/2),
                                  geom = "area"))
  }
  if(direction == ">"){ #greater than
    pltup <- initalplt +
      stat_function(fun = dnorm,args=list(mean=means,sd=sds),
                    xlim = c(quantile(x,probs = alpha),max(x)),
                    geom = "area")
    pltlw <- initalplt +
      stat_function(fun = dnorm,args=list(mean=means,sd=sds),
                    xlim = c(min(y),quantile(x,probs = alpha) ),
                    geom = "area")
  }
  if(direction == "<"){ #less than by default
    pltup <- initalplt +
      stat_function(fun = dnorm,args=list(mean=means,sd=sds),
                    xlim = c(min(x),quantile(x,probs = alpha)),
                    geom = "area")
    pltlw <- initalplt +
      stat_function(fun = dnorm,args=list(mean=means,sd=sds),
                    xlim = c(quantile(x,probs = alpha),max(x)),
                    geom = "area")
  }

  if(plotly){ #turns ggplot into plotly object
    pltup <- ggplotly(pltup)
    pltlw<- ggplotly(pltlw)
  }

  return(list(pltup, pltlw))
}
