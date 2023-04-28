# Tensorflow environment ####
library(reticulate)
use_condaenv(condaenv = "env_name",required = T)
library(keras)
library(tensorflow)
tf$constant("Hello Tensorflow!")
library(tidymodels)
library(tidytext)
library(textrecipes)


# Load the EDA script. It contains the dataset
source("Scripts/EDA.R")

# LSTM model ####
# set.seed(1234)

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
# set.seed(2354)

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

# final_metrics <- final_res %>%
#   metrics(state,  .pred_class, .pred_1)
# final_metrics

# final_res %>%
#   roc_curve(state, .pred_1) %>%
#   autoplot()

# References ####
# This dataset was published in Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.
# https://medium.com/deeplearningmadeeasy/negative-log-likelihood-6bd79b55d8b6
# https://stackoverflow.com/questions/62509324/error-loading-the-keras-package-in-r-studio