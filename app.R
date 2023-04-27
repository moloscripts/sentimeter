library(shiny)
library(shinydashboard)

source("Scripts/LSTM.R")

ui <- navbarPage("Kenya COVID-19 Vaccine Tweets", inverse = T,collapsible = T,
                 
                 # Sentiment analysis page
                 tabPanel("Sentiment analysis",
                          
                          # fluidrow to host the layout
                          fluidRow(
                            column(3,
                                   selectInput("sentiment", h5("Select sentiment"), choices = sentiment_options),
                                   dateRangeInput("daterange", h5("Date range of tweets"), 
                                                  start = ymd("2020-08-11"), 
                                                  end = ymd("2022-09-06"), 
                                                  min = min(Tweets$Date), 
                                                  max = max(Tweets$Date),
                                                  format = "yyyy-mm-dd"
                                                  ) 
                            ), 
                            column(9,
                                   tabsetPanel(type = "tabs", 
                                               # tabPanel("Tweets", DTOutput("contentDT")),
                                               tabPanel("Sentiments scores over time", plotOutput("TCtweets")), 
                                               tabPanel("Sentiment scores per location", plotOutput("locationbarplot")), 
                                               tabPanel("Sentiment scores per emotions", plotOutput("emotionplot")), 
                                               tabPanel("Tweets", DTOutput("TweetTable"))
                                   ), style='height: 1000px' 

                                   )
                          ),),
                 
                 tabPanel("LSTM model results", 
                          fluidRow(
                            h3("Percentage and count of sentiments"), 
                            valueBoxOutput("PositiveSentiments"), 
                            valueBoxOutput("NeutralSentiments"), 
                            valueBoxOutput("NegativeSentiments")
                          ), 
                          hr(),
                          
                          # Negative log likelohood plot
                          h3("Negative log likelihood"),
                          fluidRow(column(4, 
                                          h4("Description "), 
                                          "Negative log likelihood (NLL), also known as cross-entropy is a cost function that explains how  bad our machine learning model is performing based on the data.", br(),
                                          "NLL works by maximising the probability of the model predicting the correct sentiment score by minimising the negative likelihood. I.e. Maximisation by minimisation.", br(),
                                          "If the loss is high, then the errors from the prediction will also be high. 
                           This function is visualised in the first plot that has y-axis as negative values and epochs in the x-axis.",br(),
                                          "If the errors from the model are high, the negative values will also be high. However, from this plot, both the loss of the training and validation data are indicating a
                           low decline hence the model will have low errors when predicting sentiment scores."
                                          ),
                                   # Plot for Negative log likelihood
                                   column(8, h4("Plot"),  
                                          plotOutput("lstmplot"))
                                   ), 
                          hr(),
                          
                          # Accuracy plot
                          # h3("Accuracy plot"),
                          # fluidRow(column(4, 
                          #                 h4("Description"), 
                          #                 "Accuracy plot shows how accurate the model is.", br(), 
                          #                 "If the accuracy of the training data is improving and the opposite is for the validation data, the model is memorising the data rather than learning from it.", br(), 
                          #                 "Also, if the accuracy of both the training and validation data is on a steady incline in the last epochs (x-axis values) of the plot, 
                          #  then the model hasn't yet fully learned the dataset.", br(),
                          #                 "From the output, we can see the accuracy of both the training and validation dataset was on a steady incline until it reached epoch 4. 
                          #  This means that the model has almost sufficient data to learn from. Addition of more data to this model will yield the an almost similar results"
                          # ),
                          # # Plot for Negative log likelihood
                          # column(8, h4("Plot"),  
                          #        plotOutput("rocurve"))
                          # ), 
                          ),
                 
                 tabPanel("About", 
                          fluidRow(column(12, 
                                          includeMarkdown('output/about.Rmd'))))                
                 )


server <- function(input, output) {
  
  # filtered_data function for DT
  filtered_data <- reactive({
    tweet.content %>%
      filter(sentiment == input$sentiment &
               (Date >= input$daterange[1] & Date <= input$daterange[2]))
  })
  
  # DT
  output$TweetTable <- DT::renderDataTable(
    filtered_data(),  extensions = 'Buttons', 
    options = list(
      dom = 'Bfrtip',
      buttons = 
        list(list(extend = 'collection', buttons = c( 'excel', 'pdf'), text = 'Download table')),
      # Order sentiment scores (column 5) from highest to lowest
      order = list(list(5, 'desc')),
      lengthMenu = c(5,10,15),paging = TRUE,scrollX=TRUE,
      pageLength = 5, autoWidth = TRUE,fixedColumns = TRUE,scrollCollapse = T,  bFilter=1 )
  )
  
  
  # Time series render plot
  output$TCtweets <- renderPlot({
    Tweets %>%
      filter(between(Date, input$daterange[1], input$daterange[2]), sentiment %in% input$sentiment) %>%
      # filter(Tweets, sentiment %in% input$sentiment) %>%
      select(Date,`Sentiment intensity`, Location, sentiment) %>%
      group_by(Date, Location, sentiment) %>%
      summarise(`sentiment scores` = n()) %>%
      ggplot(aes(x=Date, y=`sentiment scores`, group=sentiment)) +
      geom_line(aes(color=sentiment), linewidth=0.5) +
      theme_minimal()+
      xlab("") +
      # labs(caption = "Source: Twitter") +
      theme(legend.position = "top",
            legend.text = element_text(family = "Roboto Condensed"), 
            axis.title.y =  element_text(family = "Roboto Condensed", face = "bold"), 
            axis.text =  element_text(size = 12, face = "bold", family = "Roboto Condensed"))+ 
      scale_color_discrete(name=NULL)
  })
  
  output$locationbarplot <- renderPlot({
    Tweets %>%
      filter(between(Date, input$daterange[1], input$daterange[2]), sentiment %in% input$sentiment) %>%
      select(Date,`Sentiment intensity`, Location, sentiment) %>%
      group_by(Location, sentiment) %>%
      summarise(`sentiment scores` = sum(`Sentiment intensity`)) %>%
      ggplot(aes(reorder(Location, -`sentiment scores`), `sentiment scores`, fill=sentiment)) +
      geom_col() +
      theme_minimal() +
      xlab("") +
      # labs(caption = "Source: Twitter") +
      theme(legend.position = "top",
            legend.text = element_text(family = "Roboto Condensed"), 
            axis.title.y =  element_text(family = "Roboto Condensed", face = "bold"), 
            axis.text =  element_text(size = 12, face = "bold", family = "Roboto Condensed"))+
      scale_fill_discrete(name=NULL) +
      geom_text(aes(label = `sentiment scores`), vjust = -0.5, size = 3)
    
  })
  
  # Render the emotion barplot
  output$emotionplot <- renderPlot({
    Tweets %>%
      filter(between(Date, input$daterange[1], input$daterange[2]), sentiment %in% input$sentiment) %>%
      select(Date,`Sentiment intensity`, Location, sentiment, emotion) %>%
      group_by(emotion, sentiment) %>%
      summarise(`sentiment scores` = sum(`Sentiment intensity`)) %>%
      ggplot(aes(reorder(emotion, -`sentiment scores`), `sentiment scores`, fill=sentiment)) +
      geom_col() +
      theme_minimal() +
      xlab("") +
      # labs(caption = "Source: Twitter") +
      theme(legend.position = "top",
            legend.text = element_text(family = "Roboto Condensed"), 
            axis.title.y =  element_text(family = "Roboto Condensed", face = "bold"), 
            axis.text =  element_text(size = 12, face = "bold", family = "Roboto Condensed"))+
      scale_fill_discrete(name=NULL) +
      geom_text(aes(label = `sentiment scores`), vjust = -0.5, size = 3)
  })
  


# LSTM Tab reactive options
output$PositiveSentiments <- renderValueBox({
  valueBox(positive, "Positive sentiment tweets", icon =NULL, color = "olive")
  # valueBox(positive, "Positive sentiment tweets", icon = icon("plus"), color = "olive")
  
})


# Valuebox output 
output$NeutralSentiments <- renderValueBox({
  # valueBox(neutral, "Neutral sentiment tweets", icon = icon("plus-minus"), color = "orange")
  valueBox(neutral, "Neutral sentiment tweets", icon = NULL, color = "orange")
})

output$NegativeSentiments <- renderValueBox({
  # valueBox(negative, "Negative sentiment tweets", icon = icon("minus"), color = "maroon")
  valueBox(negative, "Negative sentiment tweets", icon =NULL, color = "maroon")
  
})

# LSTM Model results function
output$lstmplot <- renderPlot({
  lstm.plot
})

output$rocurve <- renderPlot({
  roc.curve
})


}

shinyApp(ui = ui, server = server)