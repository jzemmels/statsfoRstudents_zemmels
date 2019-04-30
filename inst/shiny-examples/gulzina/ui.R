# Libraries ---------------------------------------------------------------
library(shiny)
library(plotly)
library(dplyr)
library(statsfoRdummies)




# User Interface ----------------------------------------------------------
ui <- fluidPage(
  # Application Title
  titlePanel("Bootstrapping Methods"),

  # tabs
  tabsetPanel(
    tabPanel("Bootstrapping Process",
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("Choose sample"),
                 # select method type
                 radioButtons("whichmethod", "Method", choices = c("Upload file",
                                                                   "Generate sample"),
                              selected = "Generate sample"),
                 # condition 1: file
                 conditionalPanel(
                   helpText("Upload a file"),
                   condition = "input.whichmethod == 'Upload file'",
                   # Input: Select a file
                   fileInput("file", "Choose CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   # Horizontal line
                   tags$hr(),

                   # Input: Checkbox if file has header
                   checkboxInput("header", "Header", TRUE),

                   # Input: Select separator
                   radioButtons("sep", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),

                   # Input: Select quotes
                   radioButtons("quote", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"')
                 ),
                 # condition 2: generate sample
                 conditionalPanel(
                   helpText("Sample from Normal Distribution"),
                   condition = "input.whichmethod == 'Generate sample'",
                   numericInput("mean", "Mean", value = 0),
                   numericInput("sd", "Standard deviation",
                                value = 1,
                                min = 0.0001,
                                step = 0.001)
                 ),

                 # select TRUE/FALSE for animation
                 selectInput("anime", label = h3("Animated Plot"),
                             choices = list(TRUE, FALSE),
                             selected = TRUE)
               ),

               # type of output
               mainPanel(
                 plotlyOutput("plot")
               )
             )

    ),

    tabPanel("Summary of Bootstrap Sample",
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("Choose sample"),
                 # select method type
                 radioButtons("whichmethod2", "Method", choices = c("Upload file",
                                                                   "Generate sample"),
                              selected = "Generate sample"),
                 # condition 1: file
                 conditionalPanel(
                   helpText("Upload a file"),
                   condition = "input.whichmethod2 == 'Upload file'",
                   # Input: Select a file
                   fileInput("file1", "Choose CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   # Horizontal line
                   tags$hr(),

                   # Input: Checkbox if file has header
                   checkboxInput("header1", "Header", TRUE),

                   # Input: Select separator
                   radioButtons("sep1", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),

                   # Input: Select quotes
                   radioButtons("quote1", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"')
                 ),
                 # condition 2: generate sample
                 conditionalPanel(
                   helpText("Sample from Normal Distribution"),
                   condition = "input.whichmethod2 == 'Generate sample'",
                   numericInput("mean1", "Mean", value = 0),
                   numericInput("sd1", "Standard deviation", value = 1, min = 0.0001)
                 ),
                 # how many bootsrtrap samples
                 numericInput("n", "Number of bootstrap samples: ",
                              value = 10,
                              min = 1)
               ),

               # type of output
               mainPanel(
                 plotOutput("hist")
               )
             )
    )
  )
)







