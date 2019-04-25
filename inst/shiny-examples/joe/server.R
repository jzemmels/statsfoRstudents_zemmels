library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(praise)

# Define server logic

samp <- data.frame()
means <- data.frame() #initialize an emtpy data frame to be reactively updated with sample means
totalNumSamples <- 0
allData <- data.frame()
sampleBinwidth <- 1
meansBinwidth <- 1
question <- list()

server <- function(input, output, session) {

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

  newPrompt <- observeEvent({input$newQuestion},{
    question <<- hypTest_prompt()

    shinyjs::reset("answerNullVal")
    shinyjs::reset("altVal")
    shinyjs::reset("altDir")
    shinyjs::reset("hypTestStat")
    shinyjs::reset("signifLevel")
    shinyjs::reset("hypTestDecision")
  })

  output$prompt <- eventReactive({input$newQuestion},{
    return(question$prompt)
  })

  output$nullHypVal <- renderUI({
    numericInput("answerNullVal",
                 label = "",
                 value = NULL,
                 width = "100px")
  })

  output$nullHypFeedback <- renderText({

    feedback <- case_when(input$answerNullVal == question$nullVal ~ paste("Null Hypothesis Feedback:",praise()),
                          input$answerNullVal != question$nullVal ~ "Null Hypothesis Feedback: Try again. Re-read the prompt and think about what the 'status quo' mean value is.",
                          TRUE ~ "")

    return(feedback)

    # if(is.null(input$nullVal)){
    #   return("")
    # }
    # if(input$nullVal == question$nullMean){
    #   return("Null Hypothesis Feedback: Great Job!")
    # }
    # else{
    #   return("Null Hypothesis Feedback: Try again. Re-read the prompt and think about what the 'status quo' mean value is.")
    # }
  })

  output$altHypVal <- renderUI({
    numericInput("altVal",
                 label = "",
                 value = NULL,
                 width="100px")
  })

  output$altHypDir <- renderUI({
    selectInput("altDir",
                label="",
                choices = list("",
                               intToUtf8("8800"), #8800 is HTML for "not equal to"
                               ">",
                               "<"))
  })

  output$altHypFeedback <- renderText({
    feedback <- case_when(input$altDir == question$altDir & input$altVal == question$altVal ~ paste("Alternative Hypothesis Feedback:",praise()),
                          input$altDir != question$altDir & input$altVal == question$altVal ~ "Alternative Hypothesis Feedback: Try choosing a different direction for the alternative. Re-read the prompt and look for keywords like 'less than', 'greater than', or 'not the same as'.",
                          input$altDir ==  question$altDir & input$altVal != question$altVal ~ "Alternative Hypothesis Feedback: Try a different alternative value. Remember that the alternative value is always the same as the null value that you chose above.",
                          input$altDir !=  question$altDir & input$altVal != question$altVal ~ "Alternative Hypothesis Feedback: Try choosing a different direction for the alternative. Re-read the prompt and look for keywords like 'less than', 'greater than', or 'not the same as'. Also, try a different alternative value. Remember that the alternative value is always the same as the null value that you chose above.",
                          TRUE ~ "")

    return(feedback)

    # if(is.null(input$altVal)){
    #   return("")
    # }
    # if(input$altDir == question$altDir & input$altVal == question$altMean){
    #   return("Alternative Hypothesis Feedback: Great Job!")
    # }
    # else{
    #   return("NullHypothesis Feedback: Try again. Re-read the prompt and think about what the 'status quo' mean value is.")
    # }
  })

  output$testStat <- renderUI({
    numericInput("hypTestStat",
                 label="",
                 value = NULL,
                 width = "100px")
  })

  output$testStatFeedback <- renderText({
    feedback <- case_when(input$hypTestStat >= question$testStat - .1 & input$hypTestStat <= question$testStat + .1 ~ paste("Test Statistic Feedback:",praise()),
                          input$hypTestStat <= question$testStat - .1 | input$hypTestStat >= question$testStat + .1 ~ "Test Statistic Feedback: Not quite. Remember the formula z = (xbar - mu)/(sigma/sqrt(n)) and round to two decimal placees.",
                          TRUE ~ "")

    return(feedback)
  })

  output$testSize <- renderUI({
    numericInput("signifLevel",
                 label="Signif. Level",
                 value = NULL,
                 width = "130px")
  })

  output$testSizeFeedback <- renderText({
    feedback <- case_when(input$signifLevel == question$signifLevel ~ paste("Significance Level Feedback:",praise()),
                          input$signifLevel != question$signifLevel ~ paste("Significance Level Feedback: Try again. Re-read the prompt for what the signficance level should be."),
                          TRUE ~ "")

    return(feedback)
  })

  # output$nullSD <- renderUI({
  #   numericInput("hypTestSD",
  #                label="Population Std Dev",
  #                value = 1)
  # })

  output$nullDist <- renderPlotly({
    hypTestNullDistn()
  })

  hypTestNullDistn <- eventReactive({input$plotNull},{
    plotNormal(mu = 0,
               sigma = 1,
               alpha = input$signifLevel,
               obs = input$hypTestStat,
               direction = input$altDir,
               plotly=TRUE)
  })

  output$decision <- renderUI({
    selectInput("hypTestDecision",
                label="",
                choices = list("",
                               "Reject Null",
                               "Fail to Reject Null",
                               "Accept Null",
                               "Fail to Accept Null"))
  })

  output$decisionFeedback <- renderText({
    feedback <- case_when(input$hypTestDecision == question$decision ~ paste("Hypothesis Test Decision Feedback:",praise()),
                          input$hypTestDecision == "Accept Null" ~ paste("Hypothesis Test Decision Feedback: Try again. When making a hypothesis test decision, we NEVER say that we accept the null because there is always a chance the null is actually false and we just got unlucky with the data we collected."),
                          input$hypTestDecision == "Fail to Accept Null" ~ paste("Hypothesis Test Decision Feedback: Try again. When making a hypothesis test decision, we NEVER say that we accept the null, so we would never say that we would fail to accept the null. This would be akin to saying that 'we fail to consider someone innocent.'"),
                          input$hypTestDecision == "" ~ "",
                          input$hypTestDecision != question$decision ~ paste("Hypothesis Test Decision Feedback: Try again. Does the test statistic that you calculated fall into the shaded critical region in the above normal distribution plot? How does this affect our decision?"),
                          TRUE ~ "")
    return(feedback)
  })

  output$source <- renderUI({
    sourceLink()
  })

  sourceLink <- eventReactive({input$newQuestion},{
    link <- shiny::a(question$source,href=question$source) #a is URL link tag for HTML
    return(link)
  })

  # output$conclusionSignif <- renderUI({
  #   selectInput("signif",
  #               label="",
  #               choices = list("",
  #                              "signifant",
  #                              "not significant"))
  # })
}

