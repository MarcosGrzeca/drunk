library(keras)
library(tools)

source(file_path_as_absolute("redesneurais/getDados.R"))

max_features <- 5000
maxlen = 30
outputDim = 32

dados <- getDados()
data <- processarDados(dados$textParser, maxlen, max_features)

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)
cat('Shape of label tensor:', dim(labels), "\n")

training_samples <- 3195
validation_samples <- 799
vocab_size <- max_features

source(file_path_as_absolute("redesneurais/separadorDados.R"))


story_maxlen <- map_int(all_data$story, ~length(.x)) %>% max()
#sentence <- layer_input(shape = c(story_maxlen), dtype = "int32")

sentence <- layer_input(shape = list(NULL))
encoded_sentence <- sentence %>% 
  layer_embedding(input_dim = max_features, output_dim = outputDim) %>%
  layer_dropout(rate = 0.2)

sentence <- layer_input(shape = list(NULL))
encoded_sentence_types <- sentence %>% 
  layer_embedding(input_dim = max_features, output_dim = outputDim) %>%
  layer_dropout(rate = 0.2)

merged <- list(encoded_sentence, encoded_sentence_types) %>%
  layer_add() %>%
  layer_lstm(units = outputDim) %>%
  layer_dropout(rate = 0.3)

preds <- merged %>%
  layer_dense(units = vocab_size, activation = "softmax")

model <- keras_model(inputs = list(sentence, sentence), outputs = preds)
model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)

#METODO ANTIGO
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = outputDim) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

tecnica <- "LSTM"
testes <- adicionarTeste(3, 16)
testes <- adicionarTeste(3, 32)
testes <- adicionarTeste(3, 64)
testes <- adicionarTeste(5, 16)
testes <- adicionarTeste(5, 32)
testes <- adicionarTeste(5, 64)
testes <- adicionarTeste(7, 16)
testes <- adicionarTeste(7, 32)
testes <- adicionarTeste(7, 64)
testes <- adicionarTeste(10, 16)
testes <- adicionarTeste(10, 32)
testes <- adicionarTeste(10, 64)
source(file_path_as_absolute("redesneurais/parteFinal.R"))