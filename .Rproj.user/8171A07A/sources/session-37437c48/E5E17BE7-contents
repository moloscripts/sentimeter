# Libraries and datasets ####

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


# library(keras)
# library(tensorflow)
# library(reticulate)
# 
# install.packages("tensorflow")
# library(reticulate)
# # version <- "3.9.12"
# # install_python(version)
# path_to_python <- install_python()
# virtualenv_create("r-reticulate", python = path_to_python)
# library(tensorflow)
# install_tensorflow()

options(scipen=999) 
Data <- read.csv("Data/Tweets2.csv")

# Data wrangling & EDA ####
# Create a copy
RawData <- Data

# Split the column user_location to Location
Data <- cSplit(Data, 'user_location', sep=",", type.convert=FALSE)

# Remove all the emoticons, punctuation marks, 
# weblinks and finally converts the data to a tidy structure
TidyTweets <- 
  saotd::tweet_tidy(
    DataFrame = Data)


# Investigate un-igrams, bi-grams and tri-grams
unigram.DF <- unigram(
  DataFrame = TidyTweets
)

bigram.DF <- bigram(
  DataFrame = TidyTweets
)

trigram.DF <- trigram(
  DataFrame = TidyTweets
)


# Network diagram of bigrams
bigram_network(bigram.DF, node_color = "red", set_seed = 1234, layout = "star", number = 70)

# Sentiment classifiers based on Location
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

# NLP









# Remove URL and Hashtags from the tweets text
Data$text <- gsub("http.*","",Data$text)
Data$text <- gsub("https.*","",Data$text)
Data$text <- gsub("#.*","",Data$text)
Data$text <- gsub("@.*","",Data$text)

# 

Data$text <- Data %>%
  get_sentiment(Data$text)

Data <- Data %>%
  left_join(tidytext::get_sentiments("nrc")) %>%
  mutate(sentiment = factor(sentiment))
View(Data)
  
# Create a corpus based in the Location
LocationCorpus <- Corpus(VectorSource(Data$user_location_1))
LocationDTM <-  TermDocumentMatrix(LocationCorpus)

# Create a DTM and count per each word and create a DF
LocationM <- as.matrix(LocationDTM)
count <- sort(rowSums(LocationM),decreasing=TRUE)
DF <- data.frame(word = names(count),freq=count)

# generate wordcloud
set.seed(1234)
# par(mar = rep(0, 4))
# png("wordcloud_packages.png", width=12,height=8, units='in', res=300)
# wordcloud(words = DF$word, freq = DF$freq, min.freq = 1,
#           max.words=20, random.order=FALSE, rot.per=0.15,scale=c(4,.5),
#           colors=brewer.pal(4, "Spectral"))


ggplot(data = DF, 
       aes(label = word, size = freq, col = as.character(freq))) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 14)+
  scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()

# Text analysis ####
## Bag of Words ####

set.seed(2345)
vaccine.tweets.split <- Data %>%
  filter(nchar(text)>= 15) %>%
  initial_split()

# Fetch training and testing data
training.data <- training(vaccine.tweets.split)
testing.data <- testing(vaccine.tweets.split)



# Tokenise and filter the max number of words for use in analysis
#Set max words and max length
max_words <- 2e4
max_length <- 30

tweets.vaccine.rec <- recipe(~text, data = training.data) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens=100) %>%
  step_sequence_onehot(text, sequence_length = 100)

tweets.vaccine.rec <- prep(tweets.vaccine.rec)
training.v2 <- bake(tweets.vaccine.rec, new_data = NULL, composition = "matrix")
class(training.v2)


# LSTM Model
lstm.model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words+1, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")
lstm.model


