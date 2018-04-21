library(tools)
library(keras)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"

getDados <- function() {
  dados <- query("SELECT q1 AS resposta,
                 textParser
                 FROM tweets t
                 WHERE textparser <> ''
                 AND id <> 462478714693890048
                 AND q1 IS NOT NULL
                 ")
  dados$resposta[is.na(dados$resposta)] <- 0
  dados$textParser <- enc2utf8(dados$textParser)
  dados$textParser <- iconv(dados$textParser, to='ASCII//TRANSLIT')
  return (dados)
}

processarDados <- function(textParser, maxlen, max_words) {
  onlyTexts <- textParser
  texts <- as.character(as.matrix(onlyTexts))
  tokenizer <- text_tokenizer(num_words = max_words) %>%
    fit_text_tokenizer(texts)
  
  sequences <- texts_to_sequences(tokenizer, texts)
  word_index = tokenizer$word_index
  cat("Found", length(word_index), "unique tokens.\n")
  data <- pad_sequences(sequences, maxlen = maxlen)
  
  cat("Shape of data tensor:", dim(data), "\n")
  return (data);
}

processarSequence <- function(textParser, maxlen, max_words) {
  onlyTexts <- textParser
  texts <- as.character(as.matrix(onlyTexts))
  tokenizer <- text_tokenizer(num_words = max_words) %>%
    fit_text_tokenizer(texts)
  
  sequences <- texts_to_sequences(tokenizer, texts)
  return (sequences);
}
