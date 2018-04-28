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
                 textoParserRisadaEmoticom,
                 textoParserEmoticom,
                 hashtags,
                 (SELECT GROUP_CONCAT(tn.palavra)
                   FROM tweets_nlp tn
                   WHERE tn.idTweetInterno = t.idInterno
                   GROUP BY tn.idTweetInterno) AS entidades
                 FROM tweets t
                 WHERE textparser <> ''
                 AND id <> 462478714693890048
                 AND q1 IS NOT NULL
                 ")
  dados$resposta[is.na(dados$resposta)] <- 0
  dados$textParser <- enc2utf8(dados$textParser)
  dados$textParser <- iconv(dados$textParser, to='ASCII//TRANSLIT')
  dados$textoParserRisadaEmoticom <- enc2utf8(dados$textoParserRisadaEmoticom)
  dados$textoParserRisadaEmoticom <- iconv(dados$textoParserRisadaEmoticom, to='ASCII//TRANSLIT')
  dados$textoParserEmoticom <- enc2utf8(dados$textoParserEmoticom)
  dados$textoParserEmoticom <- iconv(dados$textoParserEmoticom, to='ASCII//TRANSLIT')

  #dados$resposta[is.na(dados$resposta)] <- 0
  #dados$resposta <- as.factor(dados$resposta)
  #dados$textParser <- enc2utf8(dados$textParser)
  #dados$textParser <- iconv(dados$textParser, to='ASCII//TRANSLIT')
  #dados$hashtags = gsub("#", "#tag_", dados$hashtags)
  #dados$textParser = gsub("'", "", dados$textParser)
  #dados$numeroErros[dados$numeroErros > 1] <- 1


  dados$entidades <- enc2utf8(dados$entidades)
  dados$entidades <- iconv(dados$entidades, to='ASCII//TRANSLIT')
  dados$entidades = gsub(" ", "_", dados$entidades)
  dados$entidades = gsub(",", " ", dados$entidades)
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
  print(paste("F1 ", matriz$byClass["F1"] * 100, "Precisao ", matriz$byClass["Precision"] * 100, "Recall ", matriz$byClass["Recall"] * 100, "Acuracia ", matriz$overall["Accuracy"] * 100))
  return (results)
}

#library(tools)
#library(keras)

#set.seed(10)

#source(file_path_as_absolute("redesneurais/getDados.R"))

#resultados <- data.frame(matrix(ncol = 4, nrow = 0))
#names(resultados) <- c("Técnica", "InputDim", "OutputDim", "Epochs", "Batch", "F1", "Precisão", "Revocação", "Acurácia")

avaliacaoFinalSave <- function(model, x_test, y_test, history, tecnica, InputDim, OutputDim, features) {
  results <- model %>% evaluate(x_test, y_test)
  print(results)
  predictions <- model %>% predict_classes(x_test)
 
  matriz <- confusionMatrix(data = as.factor(predictions), as.factor(y_test), positive="1")
  print(paste("F1 ", matriz$byClass["F1"] * 100, "Precisao ", matriz$byClass["Precision"] * 100, "Recall ", matriz$byClass["Recall"] * 100, "Acuracia ", matriz$overall["Accuracy"] * 100))

  resTreinamento <- as.data.frame(history$metrics)
  treinamento_acc <- resTreinamento$acc[nrow(resTreinamento)]
  treinamento_val_acc <- resTreinamento$val_acc[nrow(resTreinamento)]
  treinamento_loss <- resTreinamento$loss[nrow(resTreinamento)]
  treinamento_val_loss <- resTreinamento$val_loss[nrow(resTreinamento)]
  epochs <- history$params$epochs
  batch_size <- history$params$batch_size

  resultados <- data.frame(matrix(ncol = 14, nrow = 0))
  tableResultados <- data.frame(tecnica, InputDim, OutputDim, features, epochs, batch_size, matriz$byClass["F1"] * 100, matriz$byClass["Precision"] * 100, matriz$byClass["Recall"] * 100, matriz$overall["Accuracy"] * 100, treinamento_acc * 100, treinamento_val_acc * 100, treinamento_loss * 100, treinamento_val_loss * 100)
  rownames(tableResultados) <- tecnica
  names(tableResultados) <- c("Técnica", "InputDim", "OutputDim", "Features", "Epochs", "Batch", "F1", "Precisão", "Revocação", "Acurácia", "Acurácia treinamento", "Acurácia validação", "Loss treinamento", "Loss validação")

  pathSave <- "redesneurais/resultados.csv"
  if (file.exists(pathSave)) {
    write.table(tableResultados, pathSave, sep = ";", col.names = F, append = T)
  } else {
    write.table(tableResultados, pathSave, sep = ";", col.names = T, append = T)
  }
  return (results)
}