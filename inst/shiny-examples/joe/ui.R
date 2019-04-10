library(shiny)
library(plotly)
library(finalProject)


shinyUI(fluidPage(title="STAT 585",
                  tabsetPanel(#widths=c(2,10),
                    tabPanel("Normal Plot",
                             sidebarLayout(
                               sidebarPanel(width=3,
                                            selectInput("distributionType",
                                                        label="Population Shape",
                                                        choices = list("Normal",
                                                                       "Another Option"),
                                                        selected="Normal"),
                                            numericInput("normalPlot_mu",
                                                         label = "Population Mean",
                                                         value=0),
                                            numericInput("normalPlot_sigma",
                                                         label = "Population Standard Deviation",
                                                         value=1),
                                            numericInput("alpha",
                                                         label = "Significance Level",
                                                         value=.05),
                                            selectInput("dir",
                                                        label="Direction of Alternative Hypothesis",
                                                        choices = list(intToUtf8("8800"), #8800 is HTML for "not equal to"
                                                                       ">",
                                                                       "<")),
                                            numericInput("obsZ",
                                                         label= "Observed Z-value",
                                                         value=0)),
                               # Show a plot of the generated distribution
                               mainPanel(
                                 plotlyOutput(outputId = "distPlot")
                               ))),
                    tabPanel("Sampling Distribution of Sample Means",
                             sidebarLayout(
                               sidebarPanel(width=3,selectInput("distributionType",
                                                                label="Population Shape",
                                                                choices = list("Normal",
                                                                               "Another Option"),
                                                                selected="Normal"),
                                            numericInput("sampleDist_mu",
                                                         label = "Population Mean",
                                                         value=0),
                                            numericInput("sampleDist_sigma",
                                                         label = "Population Standard Deviation",
                                                         value=1),
                                            textInput("name",
                                                      label = "Name of Variable",
                                                      value = "Height"),
                                            numericInput("sampleSize",
                                                         label = "Sample Size",
                                                         value=25),
                                            numericInput("numSamples",
                                                         label = "Number of Samples",
                                                         value=1),
                                            actionButton("drawSample",label = "Draw Additional Samples"),
                                            actionButton("resetMeanSample",label="Reset Samples"),
                                            downloadButton("downloadSampleData","Download Sample Data")),
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
                               ))),
                    tabPanel("Hypothesis Test Practice",
                             mainPanel(
                               h5('My hope for this tab is to have a hypothesis test "game" in which students will be given a
                                  randomly selected word problem similar to one they would see on their STAT 101 homework
                                  (I will just find these on the internet) and will
                                  need to fill out the correct hypothesis test procedure (e.g., set up their null/alternative hypotheses
                                  correctly, calculate the test statistic, etc.) to "win". They could use the Normal Plot tab to
                                  visualize their null distribution and the observed value they are given in the problem to help them
                                  understand when to reject/fail to reject the null.'),
                               h5('My current idea for the game is like "statistical MadLibs" in which students will select options
                                  from drop-down menus (e.g., <, >, \neq for the alternative distribution) or will enter values
                                  (e.g., the observed z-value). Once they fill out the form, the app will tell them whether they were
                                  right or wrong and allow them to re-try or select a new problem.')
                             )))))
