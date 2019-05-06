library(tidyverse)
library(dplyr)
library(shiny)
library(checkmate)
library(statsfoRstudents)
library(praise)

server <- function(input, output) {
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

    output$feedbackModal <- renderText({

      feedback <- case_when(input$answerModal == plotModal(randomDist)[[1]] ~ paste("Modality Feedback:",praise()),
                            input$answerModal != plotModal(randomDist)[[1]] ~ "Modality Feedback: Try again.",
                            TRUE ~ "")

      return(feedback)

    })


    output$plotShape <- renderPlot({
      plotShape(randomDist)[[2]]
    })

    output$feedbackShape <- renderText({

      feedback <- case_when(input$answerShape == plotShape(randomDist)[[1]] ~ paste("Shape Feedback:",praise()),
                            input$answerShape != plotShape(randomDist)[[1]] ~ "Shape Feedback: Try again.",
                            TRUE ~ "")

      return(feedback)

    })

    output$plotOutlier <- renderPlot({
      plotOutlier(randomDist)[[2]]
    })

    output$feedbackOutlier <- renderText({

      feedback <- case_when(input$answerOutlier == plotOutlier(randomDist)[[1]] ~ paste("Outlier Feedback:",praise()),
                            input$answerOutlier != plotOutlier(randomDist)[[1]] ~ "Outlier Feedback: Try again.",
                            TRUE ~ "")

      return(feedback)

    })

  })

}
