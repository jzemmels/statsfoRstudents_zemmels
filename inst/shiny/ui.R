library(shiny)
library(plotly)
library(statsfoRstudents)
library(shinyjs)
#library(tidyverse)
library(dplyr)
#library(checkmate)

shinyUI(fluidPage(title="STAT 585",useShinyjs(),
                  tabsetPanel(#widths=c(2,10),
                    tabPanel(title = h4("Eryn's Shiny App")),
                    tabPanel(h4("Bootstrapping Process"),
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
                    tabPanel(h4("Summary of Bootstrap Sample"),
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
                    ),
                    tabPanel(title = h4("Histogram Description Testing"),
                             sidebarPanel(
                               actionButton("redo", "New Distribution")
                             ),
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Modality",
                                          plotOutput("plotModal"),
                                          uiOutput("aspectModal")

                                 ),
                                 tabPanel("Shape",

                                          plotOutput("plotShape"),
                                          uiOutput("aspectShape")
                                 ),
                                 tabPanel("Outlier",

                                          plotOutput("plotOutlier"),
                                          uiOutput("aspectOutlier")
                                          # textOutput("feedback")
                                 )

                               )
                             )
                    ),
                    tabPanel(h4("Normal Plot"),
                             sidebarLayout(
                               sidebarPanel(width=3,
                                            selectInput("distributionType",
                                                        label=h4("Population Shape"),
                                                        choices = list("Normal",
                                                                       "Another Option"),
                                                        selected="Normal"),
                                            numericInput("normalPlot_mu",
                                                         label = h4("Population Mean"),
                                                         value=0),
                                            numericInput("normalPlot_sigma",
                                                         label = h4("Population Standard Deviation"),
                                                         value=1),
                                            numericInput("alpha",
                                                         label = h4("Significance Level"),
                                                         value=.05),
                                            selectInput("dir",
                                                        label= h4("Direction of Alternative Hypothesis"),
                                                        choices = list(intToUtf8("8800"), #8800 is HTML for "not equal to"
                                                                       ">",
                                                                       "<")),
                                            numericInput("obsZ",
                                                         label= h4("Observed Test Statistic"),
                                                         value=0)),
                               # Show a plot of the generated distribution
                               mainPanel(
                                 plotlyOutput(outputId = "distPlot",height="700px")
                               ))),
                    tabPanel(h4("Hypothesis Test Game"),
                             tags$head(
                               tags$style(
                                 HTML(
                                   "
                                   #inputs-table {
                                   border-collapse: collapse;
                                   }

                                   #inputs-table td {
                                   padding: 10px;
                                   vertical-align: bottom;
                                   }
                                   "
                                 ) #/ HTML
                               ) #/ style
                             ),
                             tags$table(id = "inputs-table"
                                        , style = "width: 100%; border-bottom: 2px solid black"
                                        , tags$tr(
                                          tags$td(style = "width: 20%; text-align: left; vertical-align: top; padding-top: 38px"
                                                  , div(
                                                    actionButton("newQuestion",label=h4("New Hypothesis Test Problem")
                                                    ) #/ actionButton
                                                  ) #/ div
                                          ) #/ td
                                          , tags$td(style = "width: 80%; text-align: left; vertical-align: top"
                                                    , div(
                                                      h3("Prompt:"),
                                                      h4(textOutput("prompt"))
                                                    ) #/ div
                                          ) #/ td
                                        )#/ tr
                             ) #/ table
                             ,tags$table(class = "col-md-11"
                                         , style = "width: 100%"
                                         , tags$tr(
                                           tags$td(style = "width: 13%; vertical-align: middle; text-align: left"
                                                   ,div(
                                                     h4(paste0("1) Null Hypothesis: ",intToUtf8("0x03BC")," ="))
                                                   ) #/ div
                                           ) #/ td
                                           , tags$td(style = "width: 5%; vertical-align: middle; text-align: left; padding-right: 10px"
                                                     ,div(
                                                       uiOutput("nullHypVal")
                                                     ) #/ div
                                           ) #/ td
                                           , tags$td(style = "width: 82%; vertical-align: middle; text-align: left"
                                                     ,div(
                                                       h5(textOutput("nullHypFeedback"))
                                                     ) #/ div
                                           ) #/ td
                                         ) #/ tr
                             ) #/ table
                             , tags$table(class = "col-md-11"
                                          , style = "width: 100%"
                                          , tags$tr(
                                            tags$td(style = "width: 15%; vertical-align: middle; text-align: left;"
                                                    ,div(
                                                      h4(paste0("2) Alternative Hypothesis: ",intToUtf8("0x03BC")))
                                                    ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 1%; vertical-align: middle; text-align: left"
                                                      ,div(style = "width: 60px"
                                                           , uiOutput("altHypDir")
                                                      ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 1%; vertical-align: middle; text-align: left; padding-right: 10px; padding-bottom: 4px"
                                                      ,div(uiOutput("altHypVal")
                                                      ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 80%; vertical-align: middle; text-align: left"
                                                      ,div(
                                                        h5(textOutput("altHypFeedback"))
                                                      ) #/ div
                                            ) #/ td
                                          ) #/ tr
                             ) #/ table
                             , tags$table(class = "col-md-11"
                                          , style = "width: 100%"
                                          , tags$tr(
                                            tags$td(style = "width: 11%; vertical-align: middle; text-align: left"
                                                    ,div(
                                                      h4("3) Test Statistic: z = ")
                                                    ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 1%; vertical-align: middle; text-align: left; padding-right: 10px; padding-bottom: 13px"
                                                      , div(
                                                        h4(uiOutput("testStat"))
                                                      ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 87%; vertical-align: middle; text-align: left"
                                                      , div(
                                                        h5(textOutput("testStatFeedback"))
                                                      ) #/ div
                                            ) #/ td
                                          ) #/ tr
                             ) #/ table
                             , tags$table(class = "col-md-11"
                                          , style = "width: 100%"
                                          , tags$tr(
                                            tags$td(style = "width: 27%; vertical-align: middle; text-align: left"
                                                    ,div(
                                                      h4("4) Compare Test Statistic to Null Distribution: ")
                                                    ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 3%; vertical-align: middle; text-align: left; padding-right: 10px"
                                                      ,div(
                                                        h4(uiOutput("testSize"))
                                                      ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 10%; vertical-align: middle; text-align: left; padding-right: 10px"
                                                      ,div(
                                                        actionButton("plotNull",h4("Plot Null Distribution"))
                                                      ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 60%; vertical-align: middle"
                                                      ,div(
                                                        h5(textOutput("testSizeFeedback"))
                                                      ) #/ div
                                            )
                                          ) #/ tr
                             ) #/ table
                             , tags$table(class = "col-md-11",
                                          style = "width: 100%"
                                          , tags$tr(
                                            tags$td(style = "width: 100%",
                                                    plotlyOutput("nullDist",height="300px")
                                            ) #/ td
                                          ) #/ tr
                             ) #/ table
                             , tags$table(class = "col-md-11"
                                          , style = "width: 100%"
                                          , tags$tr(
                                            tags$td(style = "width: 7%; vertical-align: middle; text-align: left"
                                                    ,div(
                                                      h4("5) Decision: ")
                                                    ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 3%; vertical-align: middle; text-align: left; padding-right: 10px; padding-bottom: 10px"
                                                      , div(style = "width: 200px",
                                                            h4(uiOutput("decision"))
                                                      ) #/ div
                                            )
                                            , tags$td(style = "width: 87%; vertical-align: middle"
                                                      , div(
                                                        h5(textOutput("decisionFeedback"))
                                                      ) #/ div
                                            )
                                          ) #/ tr
                             ) #/ table
                             , tags$table(class = "col-md-11"
                                          , style = "width: 100%"
                                          , tags$tr(
                                            tags$td(style = "width: 10%; vertical-align: middle; text-align: left"
                                                    ,div(
                                                      h4("Problem Source: ")
                                                    ) #/ div
                                            ) #/ td
                                            , tags$td(style = "width: 90%; vertical-align: middle; text-align: left; padding-right: 10px"
                                                      ,div(
                                                        h4(uiOutput("source"))
                                                      ) #/ div
                                            ) #/ td
                                          ) #/ tr
                             ) #/ table
                    ),

                    tabPanel(h4("Sampling Distribution of Sample Means"),
                             sidebarLayout(
                               sidebarPanel(width=3,selectInput("distributionType",
                                                                label=h4("Population Shape"),
                                                                choices = list("Normal",
                                                                               "Another Option"),
                                                                selected="Normal"),
                                            numericInput("sampleDist_mu",
                                                         label = h4("Population Mean"),
                                                         value=0),
                                            numericInput("sampleDist_sigma",
                                                         label = h4("Population Standard Deviation"),
                                                         value=1),
                                            textInput("name",
                                                      label = h4("Name of Variable"),
                                                      value = "Prices"),
                                            numericInput("sampleSize",
                                                         label = h4("Sample Size"),
                                                         value=25),
                                            numericInput("numSamples",
                                                         label = h4("Number of Samples"),
                                                         value=1),
                                            actionButton("drawSample",label = h4("Draw Additional Samples"),width = "220px"),
                                            br(),
                                            actionButton("resetMeanSample",label=h4("Reset Samples"),width = "150px"),
                                            br(),
                                            downloadButton("downloadSampleData",h4("Download Sample Data"))
                               ), #/ sidebarPanel
                               # Show a plot of the generated distribution
                               mainPanel(
                                 fluidRow(
                                   h4("Distribution of Sample Data:"),
                                   column(7,plotlyOutput(outputId = "randomSampleDist",height = "300px")),
                                   h4("Sample Summary Table:"),
                                   column(5,tableOutput("randomSampleTable"))),
                                 h4("Distribution of Sample Means:"),
                                 plotlyOutput(outputId = "meanSamplingDist",height="300px"),
                                 h4("Means Summary Table:"),
                                 tableOutput("meansTable")
                               )),
                             actionButton("showQuestions",h4("Show Questions")),
                             hidden(div(id="text_div",
                                        h4(htmlOutput("samplingDistributionQuestions",style="padding-right: 150px"))
                             )
                             )
                    )
                  )
)
)





####### Joe's old hypothesis test tab. Only madness lies past here.
# sidebarLayout(
#   sidebarPanel(width=3,
#     actionButton("newQuestion",label="New Hypothesis Test Problem")
#   ),
# column(width=9,#withMathJax(),
#        # br(),
#        h3("Prompt:"),
#        h4(textOutput("prompt")),
#        fluidRow(column(width=9,h3("1) Null Hypothesis:"))),
#        fluidRow(column(width=1,h4(paste0(intToUtf8("0x03BC")," ="),
#                                   style="padding: 0px; width: 30px;")),
#                 column(width=2,uiOutput("nullHypVal"),
#                        style="padding: 0px;"),
#                 column(width=6,textOutput("nullHypFeedback"),
#                        style="padding: 0px;")),
#        fluidRow(column(width=9,h3("2) Alternative Hypothesis:"))),
#        fluidRow(column(width=1,h4(intToUtf8("0x03BC")),
#                        style="padding: 0px; width: 20px;"),
#                 column(width=1,h4(uiOutput("altHypDir")),
#                        style="padding: 0px;"),
#                 column(width=2,h4(uiOutput("altHypVal")),
#                        style="padding: 0px;"),
#                 column(width=5,textOutput("altHypFeedback"),
#                        style="padding:0px;")),
#        fluidRow(column(width=9,h3("3) Test Statistic (within .1 of true value):"))),
#        fluidRow(column(width=1,h4("z ="),
#                        style="padding: 0px;"),
#                 column(width=2,h4(uiOutput("testStat")),
#                        style="padding: 0px;"),
#                 column(width=6,textOutput("testStatFeedback"),
#                        style="padding: 0px;")),
#        fluidRow(column(width=9,h3("4) Compare Test Statistic to Null Distribution:"))),
#        fluidRow(column(width=2,h4(uiOutput("testSize"))),
#                 # div(style="display: inline-block;width: 75px;",h4(uiOutput("nullSD"))),
#                 column(width=3,actionButton("plotNull",h4("Plot Null Distribution")),
#                        style="padding:0px;"),
#                 column(width=4,textOutput("testSizeFeedback"),
#                        style="padding:0px;")),
#        plotlyOutput("nullDist",height="300px"),
#        fluidRow(column(width=9,h3("5) Decision:"))),
#        fluidRow(column(width=3,h4(uiOutput("decision")),
#                        style="padding:0px;"),
#                 column(width=6,textOutput("decisionFeedback"),
#                        style="padding:0px;")),
#        h3("Problem Source:"),
#        h4(uiOutput("source"))#,
#        # fluidRow(column(width=9,h3("6) Conclusion:"))),
#        # fluidRow(column(width=2,h4("There is ")),
#        #          column(width=2,h4(uiOutput("conclusionSignif"))),
#        #          column(width=5,h4(" evidence to conclude that the true mean is ")))
# )
