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
training_samples <- 3197
validation_samples <- 799
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

x <- vectorize_sequences(sequences)
y <- as.numeric(as.array(dados$resposta))

indices <- sample(1:nrow(x))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):
                                (training_samples + validation_samples)]
x_train <- x[training_indices,]
y_train <- y[training_indices]
x_test <- x[validation_indices,]
y_test <- y[validation_indices]


str(dados$resposta)
str(y_train)

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(5000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  #layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

#Tentando fugir do OverFit
model <- keras_model_sequential() %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001), activation = "relu", input_shape = c(5000)) %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001), activation = "relu") %>%
  #layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model_sequential() %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l1(0.001), activation = "relu", input_shape = c(5000)) %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l1(0.001), activation = "relu") %>%
  #layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model_sequential() %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001), activation = "relu", input_shape = c(5000)) %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001), activation = "relu") %>%
  #layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(5000)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  #layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


#model <- keras_model_sequential() %>%
  #layer_embedding(input_dim = 16, output_dim = 16,
#                  input_length = 5000) %>%
  #layer_flatten() %>%
#  layer_dense(units = 16, activation = "relu") %>%
#  layer_dense(units = 16, activation = "relu") %>%
#  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train,
  y_train,
  epochs = 40,
  batch_size = 512,
  validation_split = 0.2
)

plot(history)

results <- model %>% evaluate(x_test, y_test)
results

