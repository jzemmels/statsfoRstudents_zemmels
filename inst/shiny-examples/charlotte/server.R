library(tidyverse)
server <- function(input, output) {
  
  output$aspect <- renderUI({
    if(input$description=="Modal") {
      selectInput(selected = "choose",
                  "answer", "Answer",
                  c("Uniform" = "unif", "Unimodal"= "unimod", "Bimodal"="bimod",
                    "Choose Modality"="choose")
      )
    }
  })
  
  observeEvent(input$redo, {
    randomDist <- sample(1:3,1)
      randomModality <- case_when(
        randomDist == 1 ~ c('runif(1,0,1)', "unif", '.025'),
        randomDist ==2 ~ c("rbeta(10000,2,5)", 'unimod', '.1'),
        randomDist ==3 ~ c("c(rbeta(5000,10,2), rbeta(5000,1,5))", 'bimod', '.1')
      )
      
      randomShape <- case_when(
        randomDist == 1 ~ c("rbeta(10000,2,5)", 'rs'),
        randomDist ==2 ~ c("rbeta(10000,5,2)", 'ls'),
        randomDist ==3 ~ c("rbeta(10000,5,5)", 'sym')
      )
    
    randomOutlier <- case_when(
      randomDist == 1 ~ c('runif(1,0,1)', "unif"),
      randomDist ==2 ~ c("rbeta(10000,2,5)", 'unimod'),
      randomDist ==3 ~ c("rbeta(10000,2,5) + rbeta(10000,5,2)", 'bimod')
    )
    
    output$plot <- renderPlot({
      dist <- eval(parse(text=randomModality[1]))
      dist %>%
        data.frame() %>%
        ggplot(aes(x = dist)) +
        geom_histogram(binwidth = as.numeric(randomModality[3]))
    })
    
    output$feedback <- renderText({
      print(input$answer)
      print(randomModality)
      
      
      if(input$description=="Modal")
      {
        if(input$answer==randomModality[2]) {
          return("Correct")
          #create flag (has been done)
        }
        else {"Not Correct, Select Another"}
      }
    
      
      
    })
    
  })
  


  
}