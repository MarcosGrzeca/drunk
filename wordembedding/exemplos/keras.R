imdb <- function() {
  imdb_dir <- "C:/Users/Marcos/Downloads/aclImdb"
  train_dir <- file.path(imdb_dir, "train")
  
  train_dir
  labels <- c()
  texts <- c()
  for (label_type in c("neg", "pos")) {
    label <- switch(label_type, neg = 0, pos = 1)
    dir_name <- file.path(train_dir, label_type)
    for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                             full.names = TRUE)) {
      texts <- c(texts, readChar(fname, file.info(fname)$size))
      labels <- c(labels, label)
    }
  }
  labelsTmp <- labels[1:3000]
  str(labelsTmp)
  str(dados$resposta)
}

options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT q1 AS resposta,
               textParser
               FROM tweets t
               WHERE textparser <> ''
               AND id <> 462478714693890048
               AND q1 IS NOT NULL
               ")
dados$resposta[is.na(dados$resposta)] <- 0
#dados$resposta <- as.factor(dados$resposta)

labelsTmp <- as.numeric(dados$resposta)

dados$textParser <- enc2utf8(dados$textParser)
dados$textParser <- iconv(dados$textParser, to='ASCII//TRANSLIT')
clearConsole()

onlyTexts <- dados$textParser
texts <- as.character(as.matrix(onlyTexts))
#c(t(onlyTexts))


library(keras)
#install_keras()

#library(tensorflow)
#install_tensorflow(version = "gpu")

maxlen <- 20
training_samples <- 3900
validation_samples <- 1000
max_words <- 5000
tokenizer <- text_tokenizer(num_words = max_words) %>%
  fit_text_tokenizer(texts)

sequences <- texts_to_sequences(tokenizer, texts)
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")
data <- pad_sequences(sequences, maxlen = maxlen)

labels <- as.array(labelsTmp)
##labels <- as.array(dados$resposta)
cat("Shape of data tensor:", dim(data), "\n")
cat('Shape of label tensor:', dim(labels), "\n")

indices <- sample(1:nrow(data))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):
                                (training_samples + validation_samples)]
x_train <- data[training_indices,]
y_train <- labels[training_indices]
x_val <- data[validation_indices,]
y_val <- labels[validation_indices]

#reverse_word_index <- names(word_index)
#names(reverse_word_index) <- word_index
#decoded_review <- sapply(data[[1]], function(index) {
#  word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
#  if (!is.null(word)) word else "?"
#})

#cat(decoded_review)


model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 8,
                  input_length = maxlen) %>%
  layer_flatten() %>%
  layer_dense(units = 5000, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(20)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


#model <- keras_model_sequential() %>%
#  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
#  layer_dense(units = 16, activation = "relu") %>%
#  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  #metrics = c("acc", "mae")
  metrics = c("acc")
)
#metrics=['binary_accuracy', 'fmeasure', 'precision', 'recall'])

summary(model)

history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 128,
  validation_split = 0.2
)

plot(history)

