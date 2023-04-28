#
# Project done by Andrew Molo
# molo.andrew@gmail.com
#
# 
# Sentiment analysis dashboard of COVID-19 Tweets
#
# 
# Shiny HTML UIs
# 
# https://shiny.rstudio.com/articles/html-tags.html
# https://shiny.rstudio.com/articles/tag-glossary.html
#
#




library(shiny)
library(shinydashboard)
# source("Scripts/LSTM.R")
# source("Scripts/EDA.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # custom csss
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ), 
  
  
  # Application title
  titlePanel("Sentiment Analysis of Kenya COVID-19 Vaccine Tweets"),
  hr(),
  h3("Data visualisations results"), 
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(fluid = F, position = "left",
    sidebarPanel(width = 4, 
                 br(),
                 selectInput("select", h4("Select sentiment"), choices = choices),
                 valueBox(23, subtitle=NULL, icon = NULL, color = "aqua", width = 4,href = NULL),
                 dateRangeInput("dates", h4("Date range of tweets")), 
                 br(),
                 infoBox("", value = positive, subtitle = NULL, icon = shiny::icon("bar-chart"), color = "aqua", width = 4)
                 
                 
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 8, 
              # value box
              fluidRow(
                # valueBox(positive, subtitle="positive sentiment tweets", icon = NULL, color = "aqua", width = 4,href = NULL),
                infoBox("positve sentiments: ", value = positive, subtitle = NULL, icon = shiny::icon("bar-chart"), color = "aqua", width = 4),
                valueBox(neutral, subtitle="neutral sentiment tweets", icon = NULL, color = "aqua", width = 4,href = NULL), 
                valueBox(negative, subtitle="Negative sentiment tweets", icon = NULL, color = "aqua", width = 4,href = NULL)
              ),
              hr(),
              # Tabs 
              tabsetPanel(type = "tabs", 
                          # tabPanel("Temporal tweet count", plotOutput("TCtweets")), 
                          tabPanel("Content", DTOutput("contentDT")),
                          tabPanel("Tweets over time", plotOutput("TCtweets")), 
                          tabPanel("Sentiments scores based on location", plotOutput("locationbarplot")), 
                          tabPanel("Sentiment scores based on emotions", plotOutput("emotionplot")), 
                          # tabPanel("Table of trigrams", DTOutput("trigramtable"))
                          )

    )
  ), # End of side-bar and main-panel
  hr(),
  h3("Machine learning and sentimental analysis results")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  # Render the Content
  output$contentDT <- renderDT({
    DT::datatable(tweet.content, caption = "Content", filter='top', extensions = 'Buttons', options =  list(pageLength=5,dom='Bfrtip', 
                                                                                                            buttons=c(''), 
                                                                                                            class='display'),
                  colnames = c("Content"))
  })
  
  
  # Render the line chart for tweets
  output$TCtweets <- renderPlot({
    time.series.tweets
  })
  
  # Render the location barplot
  output$locationbarplot <- renderPlot({
    loc.sentiscore
  })
  
  # Render the emotion barplot
  output$emotionplot <- renderPlot({
    emotion.sentiscore
  })
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
