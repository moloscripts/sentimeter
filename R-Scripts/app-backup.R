# Icon list. - https://fontawesome.com/icons
# Customise valuebox colors: https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors
# Look at shinydashboard plus later on https://github.com/RinteRface/shinydashboardPlus

library(shiny)
library (shinydashboard)
library(tidyverse)
library(splitstackshape)
library(tm)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(ggwordcloud)
library(textdata)
library(saotd)
library(syuzhet)
library(stringr)
library(lubridate)
library(DT)

# TF environment
library(reticulate)
use_condaenv(condaenv = "env_name",required = T)
library(keras)
library(tensorflow)
tf$constant("Hello Tensorflow!")
library(tidymodels)
library(tidytext)
library(textrecipes)


# load external scripts
source("Scripts/EDA.R")
source("Scripts/LSTM.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black", 
  
  dashboardHeader(
    title= div(h4('#KOT COVID-19 Vaccine Tweets', style="margin: 0;"), h5('Visualisations and sentiment analysis results', style="margin: 1;")), 
    titleWidth = 300, 
    
    # Increase height of the dashboard header
    tags$li(class="dropdown", 
            tags$style(".main-header {max-height: 50px}"),
            tags$style(".main-header .logo {height: 50px}"))
  ),
  
  # Sidebar
  dashboardSidebar(width = 300, 
    sidebarMenu(menuItem("Visualisations", tabName = "visualisations", icon = icon("line-chart")), 
                menuItem("Bidirectional LSTM model results", tabName = "ml", icon = icon("hashtag")), 
                menuItem("About the dashboard", tabName = "about", icon = icon("sticky-note")))
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(tabItem(tabName = "visualisations", 
                     
                     fluidRow(
                       valueBoxOutput("PositiveSentiments"), 
                       valueBoxOutput("NeutralSentiments"), 
                       valueBoxOutput("NegativeSentiments")
                       
                       # valueBox(10 * 2, "Neutral sentiment tweets", icon = icon("sentiment_neutral", lib = "glyphicon")),
                       # valueBox(7 * 2, "Negative sentiment tweets", icon = icon("minus-circle"))
                       
                     ),
                     
                     # Row 1
                     fluidRow(
                       box(width = 12, height = NULL, solidHeader = T, status = "primary", plotOutput("TCtweets"))
                     ), 
                     
                     # Row 2
                     fluidRow(
                       box(width = 7, solidHeader = T, plotOutput("locationbarplot")),
                       box(width = 5, solidHeader = T, plotOutput("WChashtags"))
                     ),
                      
                     # Row 3
                     fluidRow(
                       box(width = 6, solidHeader = T, status = "primary" ,plotOutput("WCunigrams")),
                       box(width = 6, solidHeader = T, status = "primary", plotOutput("networkbigram")),
                     ),
                     
                     # Row 4
                     fluidRow(
                       box(width = 12, solidHeader = T,  DTOutput("trigramtable"))
                     )
                     
                     ), 
             # end of visualisation tab
             
             
             # Biderictional LSTM Model results tab ####
             tabItem(tabName = "ml",
                     fluidRow(
                       box(h3("Keras training model history plot description"), width = 12, 
                           h4("Negative log likelihood"), 
                           "Negative log likelihood (NLL), also known as cross-entropy is a cost function that explains how  bad our machine learning model is performing based on the data.", br(),
                           "NLL works by maximising the probability of the model predicting the correct sentiment score by minimising the negative likelihood. I.e. Maximisation by minimisation.", br(),
                           "If the loss is high, then the errors from the prediction will also be high. 
                           This function is visualised in the first plot that has y-axis as negative values and epochs in the x-axis.",br(),
                           "If the errors from the model are high, the negative values will also be high. However, from this plot, both the loss of the training and validation data are indicating a
                           low decline hence the model will have low errors when predicting sentiment scores.", br(), 
                           
                           h4("Accuracy plot"), 
                           "Accuracy plot shows how accurate the model is.", br(), 
                           "If the accuracy of the training data is improving and the opposite is for the validation data, the model is memorising the data rather than learning from it.", br(), 
                           "Also, if the accuracy of both the training and validation data is on a steady incline in the last epochs (x-axis values) of the plot, 
                           then the model hasn't yet fully learned the dataset.", br(),
                           "From the output, we can see the accuracy of both the training and validation dataset was on a steady incline until it reached epoch 4. 
                           This means that the model has almost sufficient data to learn from. Addition of more data to this model will yield the an almost similar results"
                       ), 
                       box(title = "Keras training model history plots", width = 12, solidHeader =F , status = "primary" ,plotOutput("lstmplot"))
                     ), 
                     # RoC Curve & Confusion matrix
                     fluidRow(
                       box(title = "RoC Curve", width = 12, solidHeader =F , status = "primary" ,plotOutput("rocurve"))
                       
                     )
                     ), 
             tabItem(tabName = "about", h2("About section")))
  ),
  
  
  # Title on the browser
  title = "COVID-19 Vaccine Tweets Data Dashboard"
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$PositiveSentiments <- renderValueBox({
    valueBox(positive, "Positive sentiment tweets", icon = icon("plus"), color = "olive")
  })
  
  output$NeutralSentiments <- renderValueBox({
    valueBox(neutral, "Neutral sentiment tweets", icon = icon("plus-minus"), color = "orange")
  })
  
  output$NegativeSentiments <- renderValueBox({
    valueBox(negative, "Negative sentiment tweets", icon = icon("minus"), color = "maroon")
  })
  
  
  
  # Row 1 plots ####
  output$WChashtags <- renderPlot({
    wordcloudHashtags
  })
  
  output$WCunigrams <- renderPlot({
    unigram.wordcloud
  })
  
  
  # Row 2 plots
  output$TCtweets <- renderPlot({
    time.series.tweets
  })
  
  # Row 3 plots
  output$TCtweets <- renderPlot({
    time.series.tweets
  })
  
  # Row 4
  output$networkbigram <- renderPlot({
    bigram.network
  })
  
  output$locationbarplot <- renderPlot({
    loc.sentiscore
  })
  
  # LSTM Results
  output$lstmplot <- renderPlot({
    lstm.plot
  })
  output$rocurve <- renderPlot({
    roc.curve
  })
  
  
  # render Table
  output$trigramtable <- renderDT({
    DT::datatable(trigram.df, caption = "Table of trigrams", filter='top', extensions = 'Buttons', options =  list(pageLength=5, searching=FALSE, dom='Bfrtip', buttons=c('excel', 'pdf'), class='display'), 
                  colnames = c("1st word", "2nd word", "3rd word", "Count"))
  })
  

  
  
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
