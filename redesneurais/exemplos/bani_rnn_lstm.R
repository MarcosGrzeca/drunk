library(keras)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(tools)

tokenize_words <- function(x){
  x <- x %>% 
    str_replace_all('([[:punct:]]+)', ' \\1') %>% 
    str_split(' ') %>%
    unlist()
  x[x != ""]
}

# Function definition -----------------------------------------------------
vectorize_stories <- function(data, vocab, textParser_maxlen, hashTagg_maxlen){
  
  textParsers <- map(data$textParser, function(x){
    map_int(x, ~which(.x == vocab))
  })
  
  hashtags <- map(data$hashtags, function(x){
    map_int(x, ~which(.x == vocab))
  })

  list(
    new_textParser = pad_sequences(textParsers, maxlen = textParser_maxlen),
    new_hashtags   = pad_sequences(hashtags, maxlen = hashTagg_maxlen)
  )
}

# Parameters --------------------------------------------------------------
max_length <- 40
embed_hidden_size <- 50
batch_size <- 16
epochs <- 5

# Data Preparation --------------------------------------------------------s
max_features <- 5000

source(file_path_as_absolute("redesneurais/getDados.R"))
dados <- getDados()
labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)

dadosTransformado <- dados %>%
  mutate(
    textParser = map(textParser, ~tokenize_words(.x)),
    hashtags = map(hashtags, ~tokenize_words(.x))
  ) %>%
  select(textParser, hashtags)

#all_data <- bind_rows(train, test)
all_data <- dadosTransformado
#vocab <- c(unlist(dadosTransformado$textParser), unlist(dados$resposta),unlist(dadosTransformado$hashtags)) %>%
vocab <- c(unlist(dadosTransformado$textParser), unlist(dadosTransformado$hashtags)) %>%
  unique() %>%
  sort()

# Reserve 0 for masking via pad_sequences
vocab_size <- length(vocab) + 1
textParser_maxlen <- map_int(all_data$textParser, ~length(.x)) %>% max()
hashTagg_maxlen <- map_int(all_data$hashtags, ~length(.x)) %>% max()

train_vec <- vectorize_stories(dadosTransformado, vocab, textParser_maxlen, hashTagg_maxlen)
# Defining the model ------------------------------------------------------

sentence <- layer_input(shape = c(textParser_maxlen), dtype = "int32")
encoded_sentence <- sentence %>% 
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>% 
  layer_dropout(rate = 0.3)

question <- layer_input(shape = c(hashTagg_maxlen), dtype = "int32")
encoded_question <- question %>%
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_repeat_vector(n = textParser_maxlen)


list(encoded_sentence, encoded_question)
merged <- list(encoded_sentence, encoded_question) %>%
  layer_add() %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_dropout(rate = 0.3)

preds <- merged %>%
  #layer_dense(units = vocab_size, activation = "sigmoid")
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(inputs = list(sentence, question), outputs = preds)
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

model

# Training ----------------------------------------------------------------

loco <- list(train_vec$new_textParser, train_vec$new_hashtags)
loco <- as.array(loco)


model %>% fit(
  x = list(train_vec$new_textParser, train_vec$new_hashtags),
  #y = labels,
  y = dados$resposta,
  batch_size = batch_size,
  epochs = epochs,
  validation_split=0.05
)

model


str(list(train_vec$new_textParser, train_vec$new_hashtags))
str(dados$resposta)
str(labels)

#evaluation <- model %>% evaluate(
#  x = list(test_vec$stories, test_vec$questions),
#  y = test_vec$answers,
#  batch_size = batch_size
#)

#evaluation
