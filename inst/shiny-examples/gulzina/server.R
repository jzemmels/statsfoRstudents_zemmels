# Libraries ---------------------------------------------------------------
library(shiny)
library(plotly)
library(dplyr)
library(statsfoRstudents)




# Server ------------------------------------------------------------------
server <- function(input, output) {
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
}

