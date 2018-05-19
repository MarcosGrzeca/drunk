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
vectorize_stories <- function(data, vocab, textParser_maxlen, hashTag_maxlen, entidades_maxlen){
  
  textParsers <- map(data$textParser, function(x){
    map_int(x, ~which(.x == vocab))
  })
  
  hashtags <- map(data$hashtags, function(x){
    map_int(x, ~which(.x == vocab))
  })

  entidades <- map(data$entidades, function(x){
    map_int(x, ~which(.x == vocab))
  })

  list(
    new_textParser = pad_sequences(textParsers, maxlen = textParser_maxlen),
    new_hashtags   = pad_sequences(hashtags, maxlen = hashTag_maxlen),
    new_entidades   = pad_sequences(entidades, maxlen = entidades_maxlen)
  )
}

# Parameters --------------------------------------------------------------
max_length <- 40
embed_hidden_size <- 50
batch_size <- 16
epochs <- 3

# Data Preparation --------------------------------------------------------s
max_features <- 5000

source(file_path_as_absolute("redesneurais/getDados.R"))
dados <- getDados()
labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)

training_samples <- 3195
validation_samples <- 799
indices <- sample(1:nrow(dados))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):
                                (training_samples + validation_samples)]

x_train <- dados[training_indices,]
y_train <- labels[training_indices]
x_test <- dados[validation_indices,]
y_test <- labels[validation_indices]


dadosTransformado <- x_train %>%
  mutate(
    textParser = map(textParser, ~tokenize_words(.x)),
    hashtags = map(hashtags, ~tokenize_words(.x)),
    entidades = map(entidades, ~tokenize_words(.x))
  ) %>%
  select(textParser, hashtags, entidades)

#all_data <- bind_rows(train, test)
all_data <- dadosTransformado
#vocab <- c(unlist(dadosTransformado$textParser), unlist(dados$resposta),unlist(dadosTransformado$hashtags)) %>%
vocab <- c(unlist(dadosTransformado$textParser), unlist(dadosTransformado$hashtags), unlist(dadosTransformado$entidades)) %>%
  unique() %>%
  sort()

# Reserve 0 for masking via pad_sequences
vocab_size <- length(vocab) + 1
textParser_maxlen <- map_int(all_data$textParser, ~length(.x)) %>% max()
hashTag_maxlen <- map_int(all_data$hashtags, ~length(.x)) %>% max()
entidades_maxlen <- map_int(all_data$entidades, ~length(.x)) %>% max()

train_vec <- vectorize_stories(dadosTransformado, vocab, textParser_maxlen, hashTag_maxlen, entidades_maxlen)
# Defining the model ------------------------------------------------------

sentence <- layer_input(shape = c(textParser_maxlen), dtype = "int32")
encoded_sentence <- sentence %>% 
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>% 
  layer_dropout(rate = 0.3)

question <- layer_input(shape = c(hashTag_maxlen), dtype = "int32")
encoded_question <- question %>%
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_repeat_vector(n = textParser_maxlen)

entidades <- layer_input(shape = c(entidades_maxlen), dtype = "int32")  
encoded_entidades <- entidades %>%
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_repeat_vector(n = textParser_maxlen)

merged <- list(encoded_sentence, encoded_question, encoded_entidades) %>%
  layer_add() %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_dropout(rate = 0.3)

preds <- merged %>%
  #layer_dense(units = vocab_size, activation = "sigmoid")
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(inputs = list(sentence, question, entidades), outputs = preds)
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

#model

# Training ----------------------------------------------------------------

stop("Nao treinar")

result <- model %>% fit(
  x = list(train_vec$new_textParser, train_vec$new_hashtags, train_vec$new_entidades),
#y = labels,
  y = y_train,
  batch_size = batch_size,
  epochs = 5,
  validation_split=0.20
)


dadosTransformadoTest <- x_test %>%
  mutate(
    textParser = map(textParser, ~tokenize_words(.x)),
    hashtags = map(hashtags, ~tokenize_words(.x)),
    entidades = map(entidades, ~tokenize_words(.x))
  ) %>%
  select(textParser, hashtags, entidades)

#all_data <- bind_rows(train, test)
all_dataTest <- dadosTransformadoTest
#vocab <- c(unlist(dadosTransformadoTest$textParser), unlist(dados$resposta),unlist(dadosTransformadoTest$hashtags)) %>%
vocab <- c(unlist(dadosTransformadoTest$textParser), unlist(dadosTransformadoTest$hashtags), unlist(dadosTransformadoTest$entidades)) %>%
  unique() %>%
  sort()

# Reserve 0 for masking via pad_sequences
vocab_size <- length(vocab) + 1
textParser_maxlen_test <- map_int(all_dataTest$textParser, ~length(.x)) %>% max()
hashTag_maxlen_test <- map_int(all_dataTest$hashtags, ~length(.x)) %>% max()
entidades_maxlen_test <- map_int(all_dataTest$entidades, ~length(.x)) %>% max()

test_vec <- vectorize_stories(dadosTransformadoTest, vocab, textParser_maxlen_test, hashTag_maxlen_test, entidades_maxlen_test)


result <- model %>% fit(
  x = list(x_train$new_textParser, x_train$new_hashtags, x_train$new_entidades),
  #y = labels,
  y = y_train,
  batch_size = batch_size,
  epochs = 5,
  validation_split=0.20
)

#model

evaluation <- model %>% evaluate(
  x = list(test_vec$new_textParser, test_vec$new_hashtags, test_vec$new_entidades),
  y = test_vec$answers,
  batch_size = batch_size
)

#evaluation
