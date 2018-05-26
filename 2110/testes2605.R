resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
  load("2110/rdas/testes2605.RData")
})

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 10
registerDoMC(CORES)

treinar <- function(data_train){
    # registerDoMC(CORES)
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 5, savePred=T),
            preProc=c("center"))
    return (fit)
}

getMatriz <- function(fit, data_test) {
  # registerDoMC(CORES)
  pred <- predict(fit, subset(data_test, select = -c(resposta)))
  matriz <- confusionMatrix(data = pred, data_test$resposta, positive="1")
  return (matriz)
}

addRow <- function(resultados, baseline, matriz, ...) {
  print(baseline)
  newRes <- data.frame(baseline, matriz$byClass["F1"], matriz$byClass["Precision"], matriz$byClass["Recall"])
  rownames(newRes) <- baseline
  names(newRes) <- c("Baseline", "F1", "Precisão", "Revocação")
  newdf <- rbind(resultados, newRes)
  #save.image(file="2110/rdas/testes2605.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80


if (!exists("matrizTwoGramTypesCFS")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-cfs-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesCFS <- treinar(data_train)
    twoGramTypesCFS
    matrizTwoGramTypesCFS <- getMatriz(twoGramTypesCFS, data_test)
    resultados <- addRow(resultados, "2 Gram + Types CFS", matrizTwoGramTypesCFS)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesInfoGain")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-information-gain-hora-erro-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesInfoGain <- treinar(data_train)
    twoGramTypesInfoGain
    matrizTwoGramTypesInfoGain <- getMatriz(twoGramTypesInfoGain, data_test)
    resultados <- addRow(resultados, "2 Gram + Info Gain", matrizTwoGramTypesInfoGain)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesCFSQ2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-cfs-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesCFS <- treinar(data_train)
    twoGramTypesCFS
    matrizTwoGramTypesCFSQ2 <- getMatriz(twoGramTypesCFS, data_test)
    resultados <- addRow(resultados, "2 Gram + Types CFS Q2", matrizTwoGramTypesCFSQ2)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesInfoGain")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-information-gain-hora-erro-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesInfoGain <- treinar(data_train)
    twoGramTypesInfoGain
    matrizTwoGramTypesInfoGain <- getMatriz(twoGramTypesInfoGain, data_test)
    resultados <- addRow(resultados, "2 Gram + Info Gain + Q2", matrizTwoGramTypesInfoGain)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

limpar <- function() {
  rm("aspectos","aspectosManter","aspectosRemover","dados","dadosQ1","data_test","data_train","dataFrameEntidades","dataFrameEstabelecimento","dataFrameGram","dataFrameHash","dataFrameTexto","dbpediaTypesCompleto","dbpediaTypesPageRank","dtm_train_hash_tags","dtm_train_texto","maFinal","matriz2Gram","matriz2Gram25","matriz2GramDBPedia","matriz2GramEntidades","matriz3Gram25","matrizDBPediaCompleto","matrizDBPediaTypesPage","matrizThreeGram","matrizTwoGram25Hora","matrizTwoGramCateogoriaLocalizacao","matrizTwoGramDBPediaSubject","matrizTwoGramDiaSemana","matrizTwoGramEntidades2","matrizTwoGramEntidadesHora","matrizTwoGramEntidadesHoraErro","matrizTwoGramErros","matrizWikipedia","threeGram","treegram25","twoGDBPedia","twogram","twogram25","twoGram25Hora","twoGramCategoriaLocalizacao","twoGramDBPediaSubject","twoGramDiaSemana","twogramentidades","twoGramEntidades25","twoGramEntidadesHora","twoGramEntidadesHoraErro", "twoGramErros","vectorizerEstabelecimento","vectorizerHashTags","vocab","vocabEstabelecimento","vocabHashTags","wikipediaCategory")
  save.image(file="2110/rdas/compare22_resultado.RData")
}