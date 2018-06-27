library(keras)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(tools)

source(file_path_as_absolute("redesneurais/getDados.R"))

# Data Preparation --------------------------------------------------------s
max_features <- 5000

dados <- getDados()

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)

training_samples <- 3195
validation_samples <- 799

indices <- sample(1:nrow(dados))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):
                                (training_samples + validation_samples)]

x_train <- dados[training_indices,]
y_train <- labels[training_indices]
x_test <- dados[validation_indices,]
y_test <- labels[validation_indices]

maxlen <- 39

dados <- processarDados(x_train$textParser, maxlen, max_features)

# Parameters --------------------------------------------------------------
batch_size <- 16
epochs <- 5

# Embedding
embedding_size = 128

# Convolution
kernel_size = 5
filters = 64
pool_size = 4

# LSTM
lstm_output_size = 70

model <- keras_model_sequential()

model %>%
  layer_embedding(max_features, embedding_size, input_length = maxlen) %>%
  layer_dropout(0.25) %>%
  layer_conv_1d(
    filters, 
    kernel_size, 
    padding = "valid",
    activation = "relu",
    strides = 1
  ) %>%
  layer_max_pooling_1d(pool_size) %>%
  layer_lstm(lstm_output_size) %>%
  layer_dense(1) %>%
  layer_activation("sigmoid")

# Compile model
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Training ----------------------------------------------------------------

history <- model %>%
              fit(
                dados, y_train,
                batch_size = batch_size,
                epochs = epochs,
                validation_split = 0.2
              )