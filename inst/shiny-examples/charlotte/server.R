library(tidyverse)
library(dplyr)
library(shiny)
library(checkmate)
library(statsfoRstudents)

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

}
