source(file_path_as_absolute("redesneurais/getDados.R"))
library(keras)

maxlen = 20

dados <- getDados()
dados$alvo <- paste(dados$textParser, " ", dados$hashtags)
data <- processarDados(dados$alvo, maxlen, 5000)

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)
cat('Shape of label tensor:', dim(labels), "\n")

training_samples <- 3195
validation_samples <- 799

source(file_path_as_absolute("redesneurais/separadorDados.R"))

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 5000, output_dim = 32,
                  input_length = maxlen) %>%
  layer_flatten() %>%
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
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)

history

plot(history)

print("FINAL")
final <- avaliacaoFinal(model, x_test, y_test)
