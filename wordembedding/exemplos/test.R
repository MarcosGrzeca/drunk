library(keras)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT q1 AS resposta,
               textParser
               FROM tweets t
               WHERE textparser <> ''
               AND id <> 462478714693890048
               AND q1 IS NOT NULL
               ")
dados$resposta[is.na(dados$resposta)] <- 0
#dados$resposta <- as.factor(dados$resposta)

labelsTmp <- as.numeric(dados$resposta)

dados$textParser <- enc2utf8(dados$textParser)
dados$textParser <- iconv(dados$textParser, to='ASCII//TRANSLIT')
clearConsole()

onlyTexts <- dados$textParser
train_data <- as.character(as.matrix(onlyTexts))

#imdb <- dataset_imdb(num_words = 10000)
#train_data <- imdb$train$x
#train_labels <- imdb$train$y
#test_data <- imdb$test$x
#test_labels <- imdb$test$y


maxlen <- 20
training_samples <- 3900
validation_samples <- 1000
max_words <- 5000
tokenizer <- text_tokenizer(num_words = max_words) %>%
  fit_text_tokenizer(train_data)

sequences <- texts_to_sequences(tokenizer, train_data)
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")
data <- pad_sequences(sequences, maxlen = maxlen)

str(data)
str(sequences)

vectorize_sequences <- function(sequences, dimension = 5000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}
x_train <- vectorize_sequences(sequences)
y_train <- as.numeric(as.array(dados$resposta))

str(dados$resposta)
str(y_train)

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(5000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train,
  y_train,
  epochs = 20,
  batch_size = 512,
  validation_split = 0.1
)

plot(history)

results <- model %>% evaluate(x_test, y_test)
results
