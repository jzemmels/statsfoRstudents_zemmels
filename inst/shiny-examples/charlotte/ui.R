library(tidyverse)
library(dplyr)
library(shiny)
library(checkmate)
library(statsfoRdummies)


ui <- fluidPage(title = "Histogram Description Testing",
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
)
