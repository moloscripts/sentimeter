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



# Data ####
Data <- read.csv("Data/Tweets2.csv")
RawCopy <- Data

# Data wrangling ####

# Split the column user_location to Location
Data <- cSplit(Data, 'user_location', sep=",", type.convert=FALSE)

# Extract frequency of hashtags
Hashtags <- str_remove_all(Data$hashtags, "[^ \\w+]") |>
  str_split(" ") |> unlist() |> table()

# convert it to a dataframe
Hashtags <- as.data.frame(Hashtags)

# Replace empty strings with NA
Hashtags[Hashtags == ''] <- NA

# Remove all NA's and rename the columns
Hashtags <- Hashtags %>%
  na.omit() %>%
  rename(Hashtag = Var1,
         Count  = Freq)%>%
  filter(Count>2)



# Create a wordcloud
wordcloudHashtags <- ggplot(data = Hashtags,
                            aes(label = Hashtag, size = Count, col = as.character(Count))) +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 13)+
  labs(title = "World cloud of common hashtags")+
  theme_minimal()
# scale_color_brewer(palette = "Paired", direction = -1)+


# Convert Date column from character to Date
# Its in the format of date, month, year (dmy).
# Data$date <- dmy(Data$date)
Data$date <- as.Date(Data$date, "%d/%m/%Y")

Countoftweets <- Data %>%
  select(date) %>%
  group_by(date) %>%
  summarise(count = n())


# Line Chart of count COVID19-Tweets
time.series.tweets <- ggplot(Countoftweets, aes(x=date, y=count)) +
  # geom_line(color="#F52C65", linewidth=0.5) +
  geom_line(color="#F05F1D", linewidth=0.5) +
  theme_minimal() +
  xlab("") +
  labs(
    title = "Temporal count of COVID-19 Vaccine tweets",
    caption = "Source: Twitter"
  )


# Remove emoticons, punctuation marks, and stopwords from the dataframe
TidyTweets <- saotd::tweet_tidy(DataFrame = Data)

# Create un-igrams, bi-grams and tri-grams
unigram.df <- unigram(DataFrame = TidyTweets)
bigram.df <- bigram(DataFrame = TidyTweets)
trigram.df <- trigram(DataFrame = TidyTweets)


# Create a unigram wordcloud
unigram.df <- unigram.df %>%
  filter(n>50)
unigram.wordcloud <- ggplot(data = unigram.df,
                            aes(label = word, size = n, col = as.character(n))) +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 13)+
  labs(title = "World cloud of unigrams")+
  # scale_color_brewer(palette = "Paired", direction = -1)+
  theme_minimal()


# Network diagram of bigrams
bigram.network <- bigram_network(bigram.df, edge_color = "#00B39A" , node_color = "#F05F1D", set_seed = 1234, layout = "fr", number = 70)
#F52C65


# Derive sentiment classifiers
location.senti.score <- data_frame(id=TidyTweets$user_location_1, text = TidyTweets$text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(score = ifelse(sentiment=='positive',1,
                        ifelse(sentiment=='joy',1,
                               ifelse(sentiment=='anticipation',1,
                                      ifelse(sentiment=='trust',1,
                                             ifelse(sentiment=='surprise',1,-1)))))) %>%
  group_by(id) %>%
  summarise(total_score = sum(score)) %>%
  mutate(sentiment = ifelse(total_score>0,'positive',ifelse(total_score<0,'negative','neutral')))


# Columnplot of location-based tweets based on the sentimentscore
loc.sentiscore <- ggplot(location.senti.score, aes(reorder(id, -total_score), total_score, fill=sentiment)) +
  geom_col() +
  theme_minimal() +
  labs(x ="", y="score", title = "Sentiment scores per location", caption = "Source: Twitter") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 1))+
  geom_text(aes(label = total_score), vjust = -0.5, size = 3)


# Overall sentiscore
TidyTweets <- tibble::rowid_to_column(TidyTweets, "id")
senti.score <- data_frame(id=TidyTweets$id, text = TidyTweets$text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(score = ifelse(sentiment=='positive',1,ifelse(sentiment=='joy',1,ifelse(sentiment=='anticipation',1,ifelse(sentiment=='trust',1,ifelse(sentiment=='surprise',1,-1)))))) %>%
  group_by(id) %>%
  summarise(total_score = sum(score)) %>%
  mutate(sentiment = ifelse(total_score>0,'positive',ifelse(total_score<0,'negative','neutral')))

# get the dataframe which contains tweet message, id and it's sentiment
senti.score <- TidyTweets %>% inner_join(senti.score, by='id') %>% select('id', 'text','sentiment')

# Recode the response variable to integers
Tweets <- senti.score %>%
  mutate(score = recode(sentiment, 'negative'=-1, 'neutral'=0, 'positive'=1))

# Check the count of sentiments
table(Tweets$sentiment)
Total <-2383+764+6151
Total

negative <- round((2383/Total)*100,0)
neutral <-  round((764/Total)*100,0)
positive <-  round((6151/Total)*100,0)

negative  <- c("26% (2,380)")
neutral  <- c("8% (764)")
positive  <- c("66% (6,151)")

negative
neutral
positive


# LSTM 

# LSTM model ####
set.seed(1234)

tweets.split <- Tweets %>%
  filter(nchar(text)>=15) %>%
  initial_split()

tweets.train <- training(tweets.split)
tweets.test <- training(tweets.split)


maxwords<-5000
maxlength<-100

tw_recipe <- recipe(~text, data = tweets.train) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = maxwords) %>%
  step_sequence_onehot(text, sequence_length = maxlength)


tw_prep <- prep(tw_recipe)
tw_train <- bake(tw_prep, new_data=NULL, composition='matrix')

# Keras Model ####
# Create an LSTM model with stack 3 Bidirectional LSTM layers
# Bidirectional layers allows the LSTM network to have both forward and backward information on the sequences on each step
set.seed(1254)
final_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = maxwords + 1, output_dim = 32) %>%
  bidirectional(layer_lstm(
    units = 32, dropout = 0.4, recurrent_dropout = 0.4,
    return_sequences = TRUE
  )) %>%
  bidirectional(layer_lstm(
    units = 32, dropout = 0.4, recurrent_dropout = 0.4,
    return_sequences = TRUE
  )) %>%
  bidirectional(layer_lstm(
    units = 32, dropout = 0.4, recurrent_dropout = 0.4
  )) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile the model
final_mod %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )

# Fit the keras model to the training data
lstm.history <- final_mod %>%
  fit(
    tw_train,
    tweets.train$score,
    epochs = 10,
    validation_split = 0.25,
    batch_size = 512,
    verbose = FALSE
  )

# Model results  
lstm.plot <- plot(lstm.history)
lstm.plot


# Create a function keras_predict

keras_predict <- function(model, baked_data, response) {
  predictions <- predict(model, baked_data)[, 1]
  tibble(
    .pred_1 = predictions,
    .pred_class = if_else(.pred_1 < 0.5, 0, 1),
    state = response
  ) %>%
    mutate(across(c(state, .pred_class),            ## create factors
                  ~ factor(.x, levels = c(1, 0))))  ## with matching levels
}


# # Test the model using test data 
tw_test <- bake(tw_prep, new_data = tweets.test, composition = "matrix")
final_res <- keras_predict(final_mod, tw_test, tweets.test$score)

final_metrics <- final_res %>%
  metrics(state,  .pred_class, .pred_1)
final_metrics

roc.curve <- final_res %>%
  roc_curve(state, .pred_1) %>%
  autoplot()
roc.curve





































































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
