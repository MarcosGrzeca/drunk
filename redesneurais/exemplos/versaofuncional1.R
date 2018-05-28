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
vectorize_stories <- function(data, vocab, textParser_maxlen, entidades_maxlen){
  
  textParsers <- map(data$textEmbedding, function(x){
    map_int(x, ~which(.x == vocab))
  })
  
  entidades <- map(data$entidades, function(x){
    map_int(x, ~which(.x == vocab))
  })

  list(
    new_textParser = pad_sequences(textParsers, maxlen = textParser_maxlen),
    new_entidades   = pad_sequences(entidades, maxlen = entidades_maxlen)
  )
}

# Parameters --------------------------------------------------------------
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
    textEmbedding = map(textEmbedding, ~tokenize_words(.x)),
    entidades = map(entidades, ~tokenize_words(.x))
  ) %>%
  select(textEmbedding, entidades)

dadosTransformadoTest <- x_test %>%
  mutate(
    textEmbedding = map(textEmbedding, ~tokenize_words(.x)),
    entidades = map(entidades, ~tokenize_words(.x))
  ) %>%
  select(textEmbedding, entidades)

all_data <- bind_rows(dadosTransformado, dadosTransformadoTest)
#all_data <- dadosTransformado
vocab <- c(unlist(dadosTransformado$textEmbedding), unlist(dadosTransformado$entidades), unlist(dadosTransformadoTest$textEmbedding), unlist(dadosTransformadoTest$entidades)) %>%
  unique() %>%
  sort()

# Reserve 0 for masking via pad_sequences
vocab_size <- length(vocab) + 1
textParser_maxlen <- map_int(all_data$textEmbedding, ~length(.x)) %>% max()
entidades_maxlen <- map_int(all_data$entidades, ~length(.x)) %>% max()

textParser_maxlen
entidades_maxlen

train_vec <- vectorize_stories(dadosTransformado, vocab, textParser_maxlen, entidades_maxlen)
# Defining the model ------------------------------------------------------

sentence <- layer_input(shape = c(textParser_maxlen), dtype = "int32")
encoded_sentence <- sentence %>% 
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>% 
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_repeat_vector(n = entidades_maxlen)

entidades <- layer_input(shape = c(entidades_maxlen), dtype = "int32")  
encoded_entidades <- entidades %>%
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = embed_hidden_size)
  #%>%   layer_repeat_vector(n = textParser_maxlen)

merged <- list(encoded_sentence, encoded_entidades) %>%
  layer_add() %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_dropout(rate = 0.3)

preds <- merged %>%
  #layer_dense(units = vocab_size, activation = "sigmoid")
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(inputs = list(sentence, entidades), outputs = preds)
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

#model

# Training ----------------------------------------------------------------
#stop("Nao treinar")

history <- model %>% fit(
  x = list(train_vec$new_textParser, train_vec$new_entidades),
  y = y_train,
  batch_size = batch_size,
  epochs = 4,
  validation_split=0.20
)


#TREINADO
test_vec <- vectorize_stories(dadosTransformadoTest, vocab, textParser_maxlen, entidades_maxlen)

evaluation <- model %>% evaluate(
  x = list(test_vec$new_textParser, test_vec$new_entidades),
  y = y_test,
  batch_size = batch_size
)
evaluation

#predictions <- model %>% predict_classes(list(test_vec$new_textParser, test_vec$new_entidades))


predictions <- model %>% predict(list(test_vec$new_textParser, test_vec$new_entidades), type="class")

predictions

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

predictions <- round_df(predictions, 0)

matriz <- confusionMatrix(data = as.factor(predictions), as.factor(y_test), positive="1")
print(paste("F1 ", matriz$byClass["F1"] * 100, "Precisao ", matriz$byClass["Precision"] * 100, "Recall ", matriz$byClass["Recall"] * 100, "Acuracia ", matriz$overall["Accuracy"] * 100))

#evaluation