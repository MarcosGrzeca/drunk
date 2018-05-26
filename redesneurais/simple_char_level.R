library(keras)
library(tools)
source(file_path_as_absolute("redesneurais/getDados.R"))
maxlen <- 140
max_features = 1115000
outputDim = 0

dados <- getDados()
sequences <- processarSequenceByCharacter(dados$textParser, maxlen, 100)
labelsTmp <- as.numeric(dados$resposta)

training_samples <- 3195
validation_samples <- 799

vectorize_sequences <- function(sequences, dimension = 100) {
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
  layer_dense(units = 32, activation = "relu", input_shape = c(100)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

tecnica <- "Char NN + dropout"
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
testes <- adicionarTeste(20, 16)
testes <- adicionarTeste(20, 32)
testes <- adicionarTeste(20, 64)
source(file_path_as_absolute("redesneurais/parteFinal.R"))