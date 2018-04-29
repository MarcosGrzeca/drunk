library(keras)
library(tools)

source(file_path_as_absolute("redesneurais/getDados.R"))

maxlen = 20
dados <- getDados()

onlyTexts <- dados$textParser
texts <- as.character(as.matrix(onlyTexts))
tokenizer <- text_tokenizer(num_words = maxlen) %>%
  fit_text_tokenizer(texts)

sequences <- texts_to_sequences(tokenizer, texts)

word_index = tokenizer$word_index

sequencias <- skipgrams(texts, length(texts), window_size = 4)

#data <- processarDados(dados$textParser, maxlen, 5000)

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)
cat('Shape of label tensor:', dim(labels), "\n")


training_samples <- 3195
validation_samples <- 799

source(file_path_as_absolute("redesneurais/separadorDados.R"))

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 5000, output_dim = 16,
                  input_length = maxlen) %>%
  layer_flatten() %>%
  layer_dense(units = 5000, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)


tecnica <- "Word Embedding SkigGram"
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
source(file_path_as_absolute("redesneurais/parteFinal.R"))