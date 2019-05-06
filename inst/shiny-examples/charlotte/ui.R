library(tidyverse)
library(dplyr)
library(shiny)
library(checkmate)
library(statsfoRstudents)


ui <- fluidPage(title = "Histogram Description Testing",
                sidebarPanel(
                  actionButton("redo", "New Distribution")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Modality",
                             plotOutput("plotModal"),
                             uiOutput("aspectModal"),
                             textOutput("feedbackModal")

                    ),
                    tabPanel("Shape",

                             plotOutput("plotShape"),
                             uiOutput("aspectShape"),
                             textOutput("feedbackShape")
                    ),
                    tabPanel("Outlier",

                             plotOutput("plotOutlier"),
                             uiOutput("aspectOutlier"),
                             textOutput("feedbackOutlier")
                    )

                  )
                )
)
