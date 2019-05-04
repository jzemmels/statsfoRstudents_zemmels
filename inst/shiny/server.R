library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(praise)
#library(tidyverse)
#library(checkmate)
library(statsfoRstudents)

# Define server logic


##### Joe's global variables:
samp <- data.frame()
means <- data.frame() #initialize an emtpy data frame to be reactively updated with sample means
totalNumSamples <- 0
allData <- data.frame()
sampleBinwidth <- 1
meansBinwidth <- 1
question <- list()

server <- function(input, output, session) {

  ##### Eryn's server logic:
  reactivePlotlys <- reactive({
    ploterrors(means = input$Errors_means,
               sds = input$Errors_sds,
               alpha = input$alpha,
               direction = input$dir,
               plotly=TRUE)
  })

  output$plotup <- renderPlotly(reactivePlotlys()[[1]])
  output$plotlw <- renderPlotly(reactivePlotlys()[[2]])

  ##### Gulzina's server logic:

  output$plot <- renderPlotly({
    if (input$whichmethod == "Upload file") {
      req(input$file)
      data <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
    }
    if (input$whichmethod == "Generate sample") {
      data <- rnorm(30, mean = input$mean, sd = input$sd)
    }
    gg <- data %>%
      bootstrapProcess(anime = input$anime)
    print(gg)
  })

  output$hist <- renderPlot({
    if (input$whichmethod2 == "Upload file") {
      req(input$file1)
      data <- read.csv(input$file1$datapath,
                       header = input$header1,
                       sep = input$sep1,
                       quote = input$quote1)
    }
    if (input$whichmethod2 == "Generate sample") {
      data <- rnorm(30, mean = input$mean1, sd = input$sd1)
    }
    samples <- bootstrapSample(data, input$n)
    hh <- samples %>% lapply(mean) %>% unlist() %>%
      hist(main = "Distribution of bootstrap sample means")
    print(hh)
  })

  ##### Charlotte's server logic:

  output$aspectModal <- renderUI({
    selectInput(selected = "choose",
                "answerModal", "Answer",
                c("Uniform" = "unif", "Unimodal"= "unimod", "Bimodal"="bimod",
                  "Choose Modality"="choose")
    )
  })

  output$aspectShape <- renderUI({
    selectInput(selected = "choose",
                "answerShape", "Answer",
                c("Right-Skewed" = "rs", "Left-Skewed" = "ls", "Symmetric" = "sym",
                  "Choose Shape"="choose"))
  })

  output$aspectOutlier <- renderUI({
    selectInput(selected = "choose",
                "answerOutlier", "Answer",
                c("None" = "none", "One Outlier" = "one", "Multiple Outliers" = "multiple",
                  "Choose Outlier Number" = "choose"))
  })

  observeEvent(input$redo, {
    randomDist <- sample(1:3,1)
    output$plotModal <- renderPlot({
      plotModal(randomDist)[[2]]
    })

    output$plotShape <- renderPlot({
      plotShape(randomDist)[[2]]
    })

    output$plotOutlier <- renderPlot({
      plotOutlier(randomDist)[[2]]
    })

    # output$feedback <- renderText({
    #   print(input$answer)
    #   print(plotModal(randomDist)[[1]])
    #   if(input$answerModal==plotModal[[1]]) {
    #     return("Correct")
    #       #create flag (has been done)
    #     }
    #   else {"Not Correct, Select Another"}
    #   })
    #


  })


  ##### Joe's server logic:


  ### Normal Plot tab logic:
  output$distPlot <- renderPlotly({ #Server logic for the Normal Plot tab of the shiny app
    plotNormal(mu = input$normalPlot_mu,
               sigma = input$normalPlot_sigma,
               alpha = input$alpha,
               obs = input$obsZ,
               direction = input$dir,
               plotly=TRUE)
  })


  ### Sampling distribution of sample means logic:
  output$randomSampleDist <- renderPlotly({ #renders sample distribution bar plot
    samplePlt()
  })

  output$randomSampleTable <- renderTable({ #renders sample distribution summary stats
    sampleSummary()
  })

  output$meansTable <- renderTable({ #renders mean summary stats
    meansSummary()
  })

  output$meanSamplingDist <- renderPlotly({ #renders mean distribution bar plot
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
    isolate(allData <<- data.frame(vector(mode="numeric",length=input$sampleSize))) #only want to initialize this once (isolated)
    allData <<- data.frame(allData,samp)

    #calculates bin width based on a formula I found on Wikipedia
    sampleBinwidth <<- 2*(qnorm(.75,mean=input$sampleDist_mu,sd = input$sampleDist_sigma) -
                            qnorm(.25,mean=input$sampleDist_mu,sd = input$sampleDist_sigma))*(input$sampleSize)^(-1/3)
    meansBinwidth <<- 2*(qnorm(.75,mean=input$sampleDist_mu,sd = input$sampleDist_sigma) -
                           qnorm(.25,mean=input$sampleDist_mu,sd = input$sampleDist_sigma))/(input$sampleSize)
  })

  observeEvent(input$resetMeanSample,{ #reset the input fields of mean sampling distribution tab
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

  sampleSummary <- eventReactive({input$drawSample|input$resetMeanSample},{ # summary table for random sample
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

  meansSummary <- eventReactive({input$drawSample|input$resetMeanSample},{ #summary table for means
    tbl <- data.frame(averageOfSampleMeans = mean(means$means),
                      stdDevOfSampleMeans = sd(means$means),
                      totalNumSamples = totalNumSamples)
    tbl
  })

  # render download button for mean data
  output$downloadSampleData <- downloadHandler(filename = function(){paste0("samplingDistributionData.csv")},
                                               content = function(file){
                                                 outData <- select(allData,-1)
                                                 write.csv(x = outData,file = file,row.names = FALSE)
                                               })

  observeEvent(input$showQuestions,{
    toggle("text_div")
    output$samplingDistributionQuestions <- renderText({
      "Below is a set of questions meant to illustrate the usage of this app. The solutions are not provided in the current version of the app.

      <br> <br>

To help give meaning to our investigations, we need to set up a population to randomly
sample from and a variable to collect. In this example, we are interested in the population of 2009 Honda Fits for
      sale around Ames, IA. The variable we will examine is the listed sale price. For this investigation, we will assume
      the mean price of 2009 Honda Fits in the population is $6,741 with a standard deviation of $1,316. Also, the
      distribution of sales prices in the population is normal. Enter the information about the population in the corresponding population characteristic boxes above
      We need to enter the name for the category of interest, the assumed value for the population mean and standard deviation, and the shape of the population distribution.

      <br> <br>

      &nbsp; &nbsp; (1) Describe the population in this example.

      <br> <br>

      &nbsp; &nbsp; (2) Describe the population parameter of interest in this example. What value are we assuming for this parameter value?

      <br> <br>

      Now we need to define and study samples from this population. To begin, we will randomly sample
      5 2009 Honda Fits from our population. Enter this information in the the correct spots above.
      Then make sure the Number of Samples is set to 1.

      <br> <br>

      &nbsp; &nbsp; (3) Draw a sample from this population by clicking on the Draw Additional Samples button.
      The histogram and summary table for the sample values are given on the upper right hand
      side of the script window. Report the sample mean for this sample.

      <br> <br>

      &nbsp; &nbsp; (4) Now, draw another sample from this population by clicking on the Draw Samples button.
      Report the sample mean for this sample and compare to the previous sample's mean. Are they close in terms of thousands of dollars?

      <br> <br>

      While you were focused on the summaries on the upper right hand side of the
      script window, the summaries on the lower right hand side of the window were also changing. In this area,
      the script collects each sample mean from above, graphs them in a histogram and reports their mean and
      standard deviation. Set the Number of Samples to 100. Then click on the Draw Additional Samples button.
      The script is selecting 100 samples from the population and giving the histogram and summary statistics for each
      sample. This is the sampling distribution of the sample mean. Now draw an additional 900 samples.
      Remember: we have now taken 1000 samples of 5 2009 Honda Fits each from this population.

      <br> <br>

      &nbsp; &nbsp; (5) What is the value of the mean of the sampling distribution? Why should you have expected
      it to equal that value?

      <br> <br>

      &nbsp; &nbsp; (6) What is the value of the standard deviation of the sampling distribution? Why should you
      have expected it to equal that value? Note you may need to perform some calculations.

      <br> <br>

      &nbsp; &nbsp; (7) Briefly describe what happens to the shape of the sampling distribution when the sample size increases
      from a small sample size to a large sample size.

      <br> <br>

      &nbsp; &nbsp; (8) Briefly describe what happens to the mean of the sampling distribution when the sample size increases
      from a small sample size to a large sample size.

      <br> <br>

      &nbsp; &nbsp; (9) Briefly describe what happens to the standard deviation of the sampling distribution when the sample
      size increases from a small sample size to a large sample size."
    })
  })

  ### Hypothesis Test Challenge "game" tab logic

  newPrompt <- observeEvent({input$newQuestion},{ #resets all of the renderUI input values (if necessary) and draws a new question
    question <<- hypTest_prompt()

    shinyjs::reset("answerNullVal")
    shinyjs::reset("altVal")
    shinyjs::reset("altDir")
    shinyjs::reset("hypTestStat")
    shinyjs::reset("signifLevel")
    shinyjs::reset("hypTestDecision")
  })

  output$prompt <- eventReactive({input$newQuestion},{ #renders the actual question prompt text
    return(question$prompt)
  })

  output$nullHypVal <- renderUI({ #input for the null hypothesized value
    numericInput("answerNullVal",
                 label = "",
                 value = NULL,
                 width = "100px")
  })

  output$nullHypFeedback <- renderText({ #provides feedback based on nyllHypVal

    feedback <- case_when(input$answerNullVal == question$nullVal ~ paste("Null Hypothesis Feedback:",praise()),
                          input$answerNullVal != question$nullVal ~ "Null Hypothesis Feedback: Try again. Re-read the prompt and think about what the 'status quo' mean value is.",
                          TRUE ~ "")

    return(feedback)
  })

  output$altHypVal <- renderUI({ #input for the alternative hypothesized value (should be the same as null value)
    numericInput("altVal",
                 label = "",
                 value = NULL,
                 width="100px")
  })

  output$altHypDir <- renderUI({ #input for the direction of the alternative hypothesis
    selectInput("altDir",
                label="",
                choices = list("",
                               intToUtf8("8800"), #8800 is HTML for "not equal to"
                               ">",
                               "<"))
  })

  output$altHypFeedback <- renderText({ #feedback for the alternative hypothesis step
    feedback <- case_when(input$altDir == question$altDir & input$altVal == question$altVal ~ paste("Alternative Hypothesis Feedback:",praise()),
                          input$altDir != question$altDir & input$altVal == question$altVal ~ "Alternative Hypothesis Feedback: Try choosing a different direction for the alternative. Re-read the prompt and look for keywords like 'less than', 'greater than', or 'not the same as'.",
                          input$altDir ==  question$altDir & input$altVal != question$altVal ~ "Alternative Hypothesis Feedback: Try a different alternative value. Remember that the alternative value is always the same as the null value that you chose above.",
                          input$altDir !=  question$altDir & input$altVal != question$altVal ~ "Alternative Hypothesis Feedback: Try choosing a different direction for the alternative. Re-read the prompt and look for keywords like 'less than', 'greater than', or 'not the same as'. Also, try a different alternative value. Remember that the alternative value is always the same as the null value that you chose above.",
                          TRUE ~ "")

    return(feedback)
  })

  output$testStat <- renderUI({ #z-statistic to be calculated given information in prompt
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

  output$testSize <- renderUI({ #input for signficince level (not always .05)
    numericInput("signifLevel",
                 label="Signif. Level",
                 value = NULL,
                 width = "130px")
  })

  output$testSizeFeedback <- renderText({ #feedback for significance level
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

  output$nullDist <- renderPlotly({ #render a normal distribution plot based on the information they input above
    hypTestNullDistn()
  })

  hypTestNullDistn <- eventReactive({input$plotNull},{ #normal distribution plot based on UI input values
    plotNormal(mu = 0,
               sigma = 1,
               alpha = input$signifLevel,
               obs = input$hypTestStat,
               direction = input$altDir,
               plotly=TRUE)
  })

  output$decision <- renderUI({ #input for the hypothesis test choice
    selectInput("hypTestDecision",
                label="",
                choices = list("",
                               "Reject Null",
                               "Fail to Reject Null",
                               "Accept Null",
                               "Fail to Accept Null"))
  })

  output$decisionFeedback <- renderText({ #feedback for hypothesis test decision
    feedback <- case_when(input$hypTestDecision == question$decision ~ paste("Hypothesis Test Decision Feedback:",praise()),
                          input$hypTestDecision == "Accept Null" ~ paste("Hypothesis Test Decision Feedback: Try again. When making a hypothesis test decision, we NEVER say that we accept the null because there is always a chance the null is actually false and we just got unlucky with the data we collected."),
                          input$hypTestDecision == "Fail to Accept Null" ~ paste("Hypothesis Test Decision Feedback: Try again. When making a hypothesis test decision, we NEVER say that we accept the null, so we would never say that we would fail to accept the null. This would be akin to saying that 'we fail to consider someone innocent.'"),
                          input$hypTestDecision == "" ~ "",
                          input$hypTestDecision != question$decision ~ paste("Hypothesis Test Decision Feedback: Try again. Does the test statistic that you calculated fall into the shaded critical region in the above normal distribution plot? How does this affect our decision?"),
                          TRUE ~ "")
    return(feedback)
  })

  output$source <- renderUI({ #render the URL from which the problem was picked as a clickable link
    sourceLink()
  })

  sourceLink <- eventReactive({input$newQuestion},{
    link <- shiny::a(question$source,href=question$source) #"a" is URL link tag for HTML
    return(link)
  })

  # TODO: Include a conclusionary sentence with renderUI inputs:
  # output$conclusionSignif <- renderUI({
  #   selectInput("signif",
  #               label="",
  #               choices = list("",
  #                              "signifant",
  #                              "not significant"))
  # })
}

