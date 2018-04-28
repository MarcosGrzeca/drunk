library(tools)
library(keras)
library(ROCR)
library(caret)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"

getDados <- function() {
  dados <- query("SELECT q1 AS resposta,
                 textParser,
                 textoParserRisadaEmoticom
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

processarSequenceByCharacter <- function(textParser, maxlen, max_words) {
  onlyTexts <- textParser
  texts <- as.character(as.matrix(onlyTexts))
  tokenizer <- text_tokenizer(num_words = max_words, char_level=1) %>%
    fit_text_tokenizer(texts)
  
  #word_index = tokenizer$word_index
  #cat("Found", length(word_index), "unique tokens.\n")

  sequences <- texts_to_sequences(tokenizer, texts)
  return (sequences);
}

obterMetricas <- function(predictions, y_test) {
  pred <- prediction(predictions, y_test);

  acc.tmp <- performance(pred,"acc");
  ind = which.max(slot(acc.tmp, "y.values")[[1]])
  acc = slot(acc.tmp, "y.values")[[1]][ind]

  prec.tmp <- performance(pred,"prec");
  ind = which.max(slot(prec.tmp, "y.values")[[1]])
  prec = slot(prec.tmp, "y.values")[[1]][ind]

  rec.tmp <- performance(pred,"rec");
  ind = which.max(slot(rec.tmp, "y.values")[[1]])
  rec = slot(rec.tmp, "y.values")[[1]][ind]

  print(paste0("Acuracia ", acc))
  print(paste0("Recall ", rec))
  print(paste0("Precisao ", prec))
}

avaliacaoFinal <- function(model, x_test, y_test) {
  results <- model %>% evaluate(x_test, y_test)
  print(results)
  predictions <- model %>% predict_classes(x_test)
 
  matriz <- confusionMatrix(data = as.factor(predictions), as.factor(y_test), positive="1")
  print(paste("F1 ", matriz$byClass["F1"], "Precisao ", matriz$byClass["Precision"], "Recall ", matriz$byClass["Recall"]))
  return (results)
}