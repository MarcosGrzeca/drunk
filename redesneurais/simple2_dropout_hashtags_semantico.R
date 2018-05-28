library(keras)
library(tools)
source(file_path_as_absolute("redesneurais/getDados.R"))
maxlen <- 50
max_features = 5000
outputDim = 0

dados <- getDados()

dados$alvo <- paste(dados$textParser, " ", dados$hashtags, " ", dados$entidades)

sequences <- processarSequence(dados$alvo, maxlen, max_features)
labelsTmp <- as.numeric(dados$resposta)

training_samples <- 3195
validation_samples <- 799

vectorize_sequences <- function(sequences, dimension = max_features) {
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
  layer_dense(units = 16, activation = "relu", input_shape = c(max_features)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


#model <- keras_model_sequential() %>%
#  layer_dense(units = 8, activation = "relu", input_shape = c(max_features)) %>%
#  layer_dense(units = 8, activation = "relu") %>%
#  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

tecnica <- "Simples 2 + Hahstags + Semantic + dropout"
testes <- adicionarTeste(3, 8)
testes <- adicionarTeste(3, 16)
testes <- adicionarTeste(3, 32)
testes <- adicionarTeste(5, 8)
testes <- adicionarTeste(5, 16)
testes <- adicionarTeste(5, 32)
testes <- adicionarTeste(7, 8)
testes <- adicionarTeste(7, 16)
testes <- adicionarTeste(10, 3)
testes <- adicionarTeste(10, 16)
testes <- adicionarTeste(10, 32)
source(file_path_as_absolute("redesneurais/parteFinal.R"))