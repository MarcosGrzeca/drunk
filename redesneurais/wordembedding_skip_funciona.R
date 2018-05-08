library(readr)
library(stringr)
library(tools)
source(file_path_as_absolute("redesneurais/getDados.R"))

dados <- getDados()

reviews <- as.character(as.matrix(dados$textParser))

maxlen = 30
max_features = 5000

library(keras)
tokenizer <- text_tokenizer(num_words = max_features)
tokenizer %>% fit_text_tokenizer(reviews)

library(reticulate)
library(purrr)

skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- iter_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}

embedding_size <- 64  # Dimension of the embedding vector.
skip_window <- 5       # How many words to consider left and right.
num_sampled <- 1       # Number of negative examples to sample for each word.
outputDim <- embedding_size

input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)


embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()


dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)


#model <- keras_model_sequential() %>%
#  layer_embedding(input_dim = max_features, output_dim = outputDim) %>%
#  layer_lstm(units = 16) %>%
#  layer_dense(units = 16, activation = "relu") %>%
#  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)


model %>%
  fit_generator(
    skipgrams_generator(reviews, tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 798, epochs = 5
   )


embedding_matrix <- get_weights(model)[[1]]
embedding_matrix


model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features + 1, output_dim = embedding_size,
                  input_length = maxlen) %>%
  layer_flatten() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
summary(model)

get_layer(model, index = 1) %>%
  set_weights(list(embedding_matrix)) %>%
  freeze_weights()

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

dados <- getDados()
data <- processarDados(dados$textParser, maxlen, max_features)

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)


training_samples <- 3195
validation_samples <- 799

source(file_path_as_absolute("redesneurais/separadorDados.R"))


tecnica <- "Skig Gram"
testes <- adicionarTeste(3, 16)
testes <- adicionarTeste(3, 32)
testes <- adicionarTeste(3, 64)
testes <- adicionarTeste(3, 128)
testes <- adicionarTeste(5, 16)
testes <- adicionarTeste(5, 32)
testes <- adicionarTeste(5, 64)
testes <- adicionarTeste(5, 128)
testes <- adicionarTeste(7, 16)
testes <- adicionarTeste(7, 32)
testes <- adicionarTeste(7, 64)
testes <- adicionarTeste(7, 128)
testes <- adicionarTeste(10, 16)
testes <- adicionarTeste(10, 32)
testes <- adicionarTeste(10, 64)
testes <- adicionarTeste(10, 128)
testes <- adicionarTeste(20, 16)
testes <- adicionarTeste(20, 32)
testes <- adicionarTeste(20, 64)
testes <- adicionarTeste(20, 128)
source(file_path_as_absolute("redesneurais/parteFinal.R"))