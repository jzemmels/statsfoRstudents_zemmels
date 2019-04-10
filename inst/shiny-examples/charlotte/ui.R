ui <- fluidPage(
  sidebarPanel(
    selectInput("description", "Pick Description Type",
                c("Shape" = "Shape", "Modality"= "Modal", "Outliers" = "Outs")
    ),
    actionButton("redo", "New Distribution")
    # Only show this panel if the plot type is a histogram
    
  ),
  mainPanel(
    plotOutput("plot"),
    uiOutput('aspect'),
    textOutput('feedback')
    
  )
)