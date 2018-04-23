library(keras)
source(file_path_as_absolute("redesneurais/getDados.R"))
maxlen <- 20

dados <- getDados()
sequences <- processarSequence(dados$textParser, maxlen, 5000)
labelsTmp <- as.numeric(dados$resposta)

training_samples <- 3195
validation_samples <- 799

vectorize_sequences <- function(sequences, dimension = 5000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}
data <- vectorize_sequences(sequences)

labels <- as.numeric(as.array(dados$resposta))

indices <- sample(1:nrow(data))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):
                                (training_samples + validation_samples)]
x_train <- data[training_indices,]
y_train <- labels[training_indices]
x_test <- data[validation_indices,]
y_test <- labels[validation_indices]

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
  epochs = 5,
  batch_size = 64,
  validation_split = 0.2
)

history

plot(history)

results <- model %>% evaluate(x_test, y_test)
results