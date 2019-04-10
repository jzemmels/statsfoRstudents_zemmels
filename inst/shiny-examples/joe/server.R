library(shiny)
library(plotly)
library(finalProject)
library(dplyr)

# Define server logic

samp <- data.frame()
means <- data.frame() #initialize an emtpy data frame to be reactively updated with sample means
totalNumSamples <- 0
allData <- data.frame()
sampleBinwidth <- 1
meansBinwidth <- 1

server <- function(input, output) {

  output$distPlot <- renderPlotly({ #Server logic for the Normal Plot tab of the shiny app
    print(plotNormal(mu = input$normalPlot_mu,
                     sigma = input$normalPlot_sigma,
                     alpha = input$alpha,
                     obs = input$obsZ,
                     direction = input$dir,
                     plotly=TRUE))
  })

  output$randomSampleDist <- renderPlotly({
    samplePlt()
  })

  output$randomSampleTable <- renderTable({
    sampleSummary()
  })

  output$meansTable <- renderTable({
    meansSummary()
  })

  output$meanSamplingDist <- renderPlotly({
    meansPlt()
  })

  observeEvent(input$drawSample,{ #Anytime the buttom is clicked, creates a new random sample
    samp <<- randomSample(mu=input$sampleDist_mu,
                            sigma = input$sampleDist_sigma,
                            sampleSize=input$sampleSize,
                            numSamples=input$numSamples) #Updates the global sample dataframe
    totalNumSamples <<- as.integer(totalNumSamples + input$numSamples) #update with total number of samples drawn since starting the app
    means <<- updateSampleMeans(sampleMeans = means,
                                sampleData = samp) #Updates the global means dataframe
    isolate(allData <<- data.frame(vector(mode="numeric",length=input$sampleSize)))
    allData <<- data.frame(allData,samp)

    sampleBinwidth <<- 2*(qnorm(.75,mean=input$sampleDist_mu,sd = input$sampleDist_sigma) -
                            qnorm(.25,mean=input$sampleDist_mu,sd = input$sampleDist_sigma))*(input$sampleSize)^(-1/3)
    meansBinwidth <<- 2*(qnorm(.75,mean=input$sampleDist_mu,sd = input$sampleDist_sigma) -
                           qnorm(.25,mean=input$sampleDist_mu,sd = input$sampleDist_sigma))/(input$sampleSize)
  })

  observeEvent(input$resetMeanSample,{
    means <<- data.frame()
    samp <<- data.frame()
    totalNumSamples <<- 0
    allData <<- data.frame()
  })

  samplePlt <- eventReactive({input$drawSample|input$resetMeanSample},{ #Updates the sample histogram when the drawSample button is pressed
    randomSample_histogram(sampleData = samp,
                           variableName = input$name,
                           binwidth = sampleBinwidth,
                           plotly=TRUE) #Updates histogram of random sample
  })

  sampleSummary <- eventReactive({input$drawSample|input$resetMeanSample},{
    dat <- samp[,ncol(samp)]

    tbl <- data.frame(Mean = mean(dat),
                      stdDev = sd(dat),
                      sampleNum = totalNumSamples)
    return(tbl)
  })

  meansPlt <- eventReactive({input$drawSample|input$resetMeanSample},{ #Updates the mean sampling distribution histogram when the drawSample button is pressed
    sampleMeans_histogram(means,
                          variableName = input$name,
                          binwidth=meansBinwidth,
                          plotly=TRUE) #Updates histogram of means
  })

  meansSummary <- eventReactive({input$drawSample|input$resetMeanSample},{
    tbl <- data.frame(averageOfSampleMeans = mean(means$means),
                      stdDevOfSampleMeans = sd(means$means),
                      totalNumSamples = totalNumSamples)
    tbl
  })

  output$downloadSampleData <- downloadHandler(filename = function(){paste0("samplingDistributionData.csv")},
                                               content = function(file){
                                                 outData <- select(allData,-1)
                                                 write.csv(x = outData,file = file,row.names = FALSE)
                                                 })
}
