
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
library(ggwordcloud)


# List of dashboard colors to use


# Data ####
Data <- read.csv("Data/Tweets2.csv")
RawCopy <- Data

# Data wrangling ####

## Split the column user_location to Location ####
Data <- cSplit(Data, 'user_location', sep=",", type.convert=FALSE)

# Extract frequency of hashtags
# Data <- Data %>%
#   mutate(Hashtags = str_remove_all(Data$hashtags, "[^ \\w+]"))

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

# View(Hashtags)

# Create a wordcloud
wordcloudHashtags <- ggplot(data = Hashtags,
       aes(label = Hashtag, size = Count, col = as.character(Count))) +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 13)+
  labs(title = "World cloud of common hashtags")+
  theme_minimal()
  # scale_color_brewer(palette = "Paired", direction = -1)+
wordcloudHashtags

# Convert Date column from character to Date
# Its in the format of date, month, year (dmy).
# Data$date <- dmy(Data$date)
Data$date <- as.Date(Data$date, "%d/%m/%Y")


## Remove emoticons, punctuation marks, and stopwords from the dataframe ####
TidyTweets <- saotd::tweet_tidy(DataFrame = Data)

# Create un-igrams, bi-grams and tri-grams
# unigram.df <- unigram(DataFrame = TidyTweets)
# bigram.df <- bigram(DataFrame = TidyTweets)
# trigram.df <- trigram(DataFrame = TidyTweets)
# 
# 
# # Create a unigram wordcloud
# unigram.df <- unigram.df %>%
#   filter(n>50)
# unigram.wordcloud <- ggplot(data = unigram.df,
#                             aes(label = word, size = n, col = as.character(n))) +
#   geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
#                       grid_size = 1, eccentricity = .9)+
#   scale_size_area(max_size = 13)+
#   labs(title = "World cloud of unigrams")+
#   # scale_color_brewer(palette = "Paired", direction = -1)+
#   theme_minimal()
# 
# unigram.df


# Network diagram of bigrams
# bigram.network <- bigram_network(bigram.df, edge_color = "#00B39A" , node_color = "#F05F1D", set_seed = 1234, layout = "fr", number = 70)
#F52C65


## Overall Dataframe with Sentiment classification ####
TidyTweets <- tibble::rowid_to_column(TidyTweets, "id")
senti.score <- data_frame(id=TidyTweets$id, text = TidyTweets$text, loc=TidyTweets$user_location_1,  datee = TidyTweets$date) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(score = ifelse(sentiment=='positive',1,ifelse(sentiment=='joy',1,ifelse(sentiment=='anticipation',1,ifelse(sentiment=='trust',1,ifelse(sentiment=='surprise',1,-1)))))) %>%
  group_by(id, datee, loc) %>%
  summarise(total_score = sum(score)) %>%
  mutate(sentiment = ifelse(total_score>0,'Positive',ifelse(total_score<0,'Negative','Neutral')))

# get the dataframe which contains tweet message, id and it's sentiment
senti.score <- TidyTweets %>% 
  inner_join(senti.score, by='id') %>% 
  select('id', 'text','sentiment','datee','loc', 'total_score')


# Recode the response variable to integers & rename some variables ####
Tweets <- senti.score %>%
  mutate(score = recode(sentiment, 'Negative'=-1, 'Neutral'=0, 'Positive'=1)) %>%
  rename(Date = datee,
         Location=loc, 
         `Sentiment intensity` = total_score)


# Create a list of the sentiments to be used in the shiny app
choices <- unique(Tweets$sentiment)


# Data visualisation ####
## Line Chart: Sentiment intenisty over time ####
time.series.tweets <- Tweets %>%
  select(Date,`Sentiment intensity`, Location, sentiment) %>%
  group_by(Date, Location, sentiment) %>%
  summarise(`sentiment scores` = n()) %>%
  ggplot(aes(x=Date, y=`sentiment scores`, group=sentiment)) +
  geom_line(aes(color=sentiment), linewidth=0.5) +
  theme_minimal()+
  xlab("") +
  labs(
    caption = "Source: Twitter"
  ) +
  theme(legend.position = "top") +
  scale_color_discrete(name=NULL)


## Column Chart: Sentiment scores per location ####
loc.sentiscore <- Tweets %>%
  select(Date,`Sentiment intensity`, Location, sentiment) %>%
  group_by(Location, sentiment) %>%
  summarise(`sentiment scores` = sum(`Sentiment intensity`)) %>%
  ggplot(aes(reorder(Location, -`sentiment scores`), `sentiment scores`, fill=sentiment)) +
  geom_col() +
  theme_minimal() +
  xlab("") +
  labs(caption = "Source: Twitter") +
  theme(legend.position = "top") +
  scale_fill_discrete(name=NULL) +
  geom_text(aes(label = `sentiment scores`), vjust = -0.5, size = 3)


## Data Table of sentiments
tweet.content <- Tweets %>%
  select(text) %>%
  distinct(text)
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


