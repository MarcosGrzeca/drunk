library(tools)
source(file_path_as_absolute("redesneurais/getDados.R"))
library(keras)

maxlen = 30
max_features = 5000
outputDim = 100

dados <- getDados()
#data <- processarDados(dados$textParser, maxlen, 5000)

onlyTexts <- dados$textParser
texts <- as.character(as.matrix(onlyTexts))
tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(texts)

sequences <- texts_to_sequences(tokenizer, texts)
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")
data <- pad_sequences(sequences, maxlen = maxlen)

labelsTmp <- as.numeric(dados$resposta)
labels <- as.array(labelsTmp)
cat('Shape of label tensor:', dim(labels), "\n")

training_samples <- 3195
validation_samples <- 799

source(file_path_as_absolute("redesneurais/separadorDados.R"))


#Montando SkipGram
library(text2vec)

dadosSkipGram <- getDados()
tokens <- dadosSkipGram$textParser %>% tolower %>%  word_tokenizer
# create vocabulary
it = itoken(tokens)
#v <- create_vocabulary(it, stopwords = tm::stopwords("en")) %>% prune_vocabulary(term_count_min = 2)
v <- create_vocabulary(it)

vectorizer <- vocab_vectorizer(v)
#vectorizer = vocab_vectorizer(v, grow_dtm = F, skip_grams_window = 5)

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

embedding_dim <- 100
glove = GlobalVectors$new(word_vectors_size = embedding_dim, vocabulary = v, x_max = maxlen)
word_vectors_main <- glove$fit_transform(tcm, n_iter = 10)

word_vectors_context = glove$components
word_vectorsSkip = word_vectors_main + t(word_vectors_context)
word_vectorsSkip

nroErros <- 0 

embedding_matrix <- array(0, c(max_features, embedding_dim))
for (word in names(word_index)) {
  index <- word_index[[word]]
  if (index < max_features) {
    #embedding_vector <- word_vectorsSkip[[word]]
    #print(word)
    try({
      #embedding_vector <- word_vectorsSkip[word, , drop = FALSE]
      tryCatch(embedding_vector <- word_vectorsSkip[word, , drop = FALSE], error=function(e) print(word))
      
      if (!is.null(embedding_vector))
        embedding_matrix[index+1,] <- embedding_vector
    })
  }
}

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = embedding_dim,
                  input_length = maxlen) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 32, activation = "relu") %>%
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

tecnica <- "Word Embedding pré-treinado SkipGram"
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


history <- model %>% fit(
  x_train, y_train,
  epochs = 5,
  batch_size = 32,
  validation_split = 0.2
)