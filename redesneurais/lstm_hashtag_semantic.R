library(tools)
source(file_path_as_absolute("redesneurais/getDados.R"))
library(keras)

set.seed(10)

maxlen = 40
max_features = 5000
outputDim = 16

dados <- getDados()
dados$alvo <- paste(dados$textParser, " ", dados$hashtags, " ", dados$entidades)
data <- processarDados(dados$alvo, maxlen, max_features)

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)
cat('Shape of label tensor:', dim(labels), "\n")

training_samples <- 3195
validation_samples <- 799

source(file_path_as_absolute("redesneurais/separadorDados.R"))

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = outputDim) %>%
  layer_lstm(units = 16) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

tecnica <- "LSTM + HashTag + Enriquecimento Semantico"
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