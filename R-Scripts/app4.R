ui <- fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        "bins", label = "Number of bins:",
        min = 1, value = 30, max = 50
      )
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)