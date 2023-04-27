
# Libraries ####
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
library(ggiraph)

# library(plotly)
# library(showtext)
# 
# # library(extrafont)
# # font_import("/Users/andrewmolo/Library/Fonts")
# # fonts()
# # fonttable()
# 
# font_add_google('Roboto Condensed', 'Roboto Condensed')
# showtext_auto()
# Data ####
Data <- read.csv("Data/Tweets2.csv")
RawCopy <- Data

# Data wrangling ####

# Split the column user_location to Location
Data <- cSplit(Data, 'user_location', sep=",", type.convert=FALSE)

# Removing Hashtags
Hashtags <- str_remove_all(Data$hashtags, "[^ \\w+]") |>
  str_split(" ") |> unlist() |> table()

# convert it to a dataframe
Hashtags <- as.data.frame(Hashtags)
# View(Hashtags)

# Replace empty strings with NA
Hashtags[Hashtags == ''] <- NA


# Remove all NA's and rename the columns
Hashtags <- Hashtags %>%
  na.omit() %>%
  rename(Hashtag = Var1,
         Count  = Freq)%>%
  filter(Count>2)

# Convert Date column from character to Date
Data$date <- as.Date(Data$date, "%d/%m/%Y")

# Remove emoticons, punctuation marks, and stop words from the DF
TidyTweets <- saotd::tweet_tidy(DataFrame = Data)

# Create unigrams Bigrams
unigram.df <- unigram(DataFrame = TidyTweets)
bigram.df <- bigram(DataFrame = TidyTweets)


# Overall DF with Sentiment classification
TidyTweets <- tibble::rowid_to_column(TidyTweets, "id")

senti.score <- data_frame(id=TidyTweets$id, text = TidyTweets$text, loc=TidyTweets$user_location_1,  datee = TidyTweets$date) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(score = ifelse(sentiment=='positive',1,ifelse(sentiment=='joy',1,ifelse(sentiment=='anticipation',1,ifelse(sentiment=='trust',1,ifelse(sentiment=='surprise',1,-1)))))) %>%
  mutate(emotion = sentiment) %>%
  group_by(id, datee, loc, emotion) %>%
  summarise(total_score = sum(score)) %>%
  mutate(sentiment = ifelse(total_score>0,'Positive',ifelse(total_score<0,'Negative','Neutral')))

# Get the  which contains tweet content & sentiment
senti.score <- TidyTweets %>% 
  inner_join(senti.score, by='id') %>% 
  dplyr::select('id', 'text','sentiment','datee','loc', 'total_score', 'emotion')



# Recode the response variable to integers & rename some columns
Tweets <- senti.score %>%
  mutate(score = recode(sentiment, 'Negative'=-1, 'Neutral'=0, 'Positive'=1)) %>%
  rename(Date = datee,
         Location=loc, 
         `Sentiment intensity` = total_score)

# Create a unique list of the sentiments to be used in the shiny app
sentiment_options <- unique(Tweets$sentiment)

# Data viz ####

# Wordcloud: Word Cloud of Hashtags 
wordcloudHashtags <- ggplot(data = Hashtags,
                            aes(label = Hashtag, size = Count, col = as.character(Count))) +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 13)+
  labs(title = "World cloud of common hashtags")+
  theme_minimal()
# scale_color_brewer(palette = "Paired", direction = -1)+
wordcloudHashtags



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
unigram.wordcloud

# Create a bigram network
bigram.network <- bigram_network(bigram.df, edge_color = "#00B39A" , node_color = "#F05F1D", set_seed = 1234, layout = "fr", number = 70)
bigram.network



# par(mfrow=c(3,1))
# wordcloudHashtags
# unigram.wordcloud
# bigram.network


# Line Chart: Sentiment intensity over time
time.series.tweets <- Tweets %>%
  dplyr::select(Date,`Sentiment intensity`, Location, sentiment) %>%
  group_by(Date, Location, sentiment) %>%
  summarise(`sentiment scores` = n()) %>%
  ggplot(aes(x=Date, y=`sentiment scores`, group=sentiment)) +
  geom_line_interactive(aes(color=sentiment), linewidth=0.5)+ 
  # geom_line(aes(color=sentiment), linewidth=0.5) +
  theme_minimal(base_size = 11)+
  scale_color_manual(values =c("#C41C0A","#3EC9CE"))+
  xlab("") +
  # labs(caption = "Source: Twitter") +
  theme(legend.position = "top",
        legend.text = element_text(family = "Roboto Condensed"), 
        axis.title.y =  element_text(family = "Roboto Condensed", face = "bold"), 
        axis.text =  element_text(size = 12, face = "bold", family = "Roboto Condensed"))+ 
  scale_color_discrete(name=NULL)



# Column Chart: Sentiment scores per location
loc.sentiscore <- Tweets %>%
  dplyr::select(Date,`Sentiment intensity`, Location, sentiment) %>%
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

# Column Chart: sentiment scores per emotion 
emotion.sentiscore <- Tweets %>%
  dplyr::select(Date,`Sentiment intensity`, Location, sentiment, emotion) %>%
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


# library(ggpubr)
# combinedPlot <- ggarrange(loc.sentiscore, emotion.sentiscore, nrow = 1, ncol = 2)
# combinedPlot

## DT of tweets
tweet.content <- Tweets %>%
  dplyr::select(Date, text, emotion, sentiment, score) %>%
  group_by(Date, sentiment, text, emotion) %>%
  summarise(sentiment_score = sum(score)) %>%
  rename(Tweet = text)

# Calculate the percentage and count of sentiments
table(Tweets$sentiment)
Total <-2383+764+6151


negative <- round((2383/Total)*100,0)
neutral <-  round((764/Total)*100,0)
positive <-  round((6151/Total)*100,0)

negative  <- c("26% (2,380)")
neutral  <- c("8% (764)")
positive  <- c("66% (6,151)")

# negative  <- c("26%")
# neutral  <- c("8%")
# positive  <- c("66%")


