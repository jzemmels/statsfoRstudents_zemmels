library(shiny)
require(ggplot2)
require(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Type I Error vs Type II error"),
  
  # Sidebar with a slider input 
  sidebarPanel(
    numericInput("means", "Mean: ", value=0),
    numericInput("sds", "Standard deviation:",
                 value = 1, min=0.0001),
    numericInput("alpha", "Alpha Level", value=0.5, min=0.00000001, max=0.49),
    selectInput("dir",
                label="Direction of Alternative Hypothesis",
                choices = list(intToUtf8("8800"), #8800 is HTML for "not equal to"
                               ">",
                               "<"))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    h4("Type I") ,
    plotOutput("plotup"),
    h4("Type II"),
    plotOutput("plotlw")
  )
)