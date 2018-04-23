library(keras)
library(tools)

source(file_path_as_absolute("redesneurais/getDados.R"))

max_features <- 5000
maxlen = 20
dados <- getDados()
data <- processarDados(dados$textParser, maxlen, max_features)

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)
cat('Shape of label tensor:', dim(labels), "\n")

training_samples <- 3195
validation_samples <- 799

source(file_path_as_absolute("redesneurais/separadorDados.R"))

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)
history <- model %>% fit(
  x_train, y_train,
  epochs = 5,
  batch_size = 64,
  validation_split = 0.2
)
history
plot(history)

results <- model %>% evaluate(x_test, y_test)
results