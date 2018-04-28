library(tools)
source(file_path_as_absolute("redesneurais/getDados.R"))
library(keras)

set.seed(10)

maxlen = 40
max_features = 9000

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
  layer_embedding(input_dim = max_features, output_dim = 16) %>%
  layer_lstm(units = 16) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

summary(model)

history <- model %>% fit(
  x_train, y_train,
  epochs = 7,
  batch_size = 64,
  validation_split = 0.2
)

print("FINAL")
final <- avaliacaoFinalSave(model, x_test, y_test, history, "LSTM", 0, 0, 9000)
