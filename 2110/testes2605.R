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
            trControl = trainControl(method = "cv", number = 10, savePred=T),
            preProc=c("center"))
    return (fit)
}

treinarFolds <- function(data_train, numFolds){
    # registerDoMC(CORES)
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = numFolds, savePred=T),
            preProc=c("center"))
    return (fit)
}

treinarPoly <- function(data_train){
    # registerDoMC(CORES)
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmPoly", 
            trControl = trainControl(method = "cv", number = 10, savePred=T),
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


if (!exists("matrizTwoGramTypesCFSV2")) {
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

if (!exists("matrizTwoGramTypesInfoGainV2")) {
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

if (!exists("matrizTwoGramTypesCFSQ2V2")) {
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

if (!exists("matrizTwoGramTypesInfoGainQ2V2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-information-gain-hora-erro-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesInfoGain <- treinar(data_train)
    twoGramTypesInfoGain
    matrizTwoGramTypesInfoGainQ2 <- getMatriz(twoGramTypesInfoGain, data_test)
    resultados <- addRow(resultados, "2 Gram + Info Gain + Q2", matrizTwoGramTypesInfoGainQ2)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesCFSQ2PolyV2V2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-cfs-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesCFS <- treinarPoly(data_train)
    twoGramTypesCFS
    matrizTwoGramTypesCFSQ2PolyV2 <- getMatriz(twoGramTypesCFS, data_test)
    resultados <- addRow(resultados, "2 Gram + Types CFS Q2 (Poly V2)", matrizTwoGramTypesCFSQ2PolyV2)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesInfoGainPolyV2V2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-information-gain-hora-erro-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesInfoGain <- treinarPoly(data_train)
    twoGramTypesInfoGain
    matrizTwoGramTypesInfoGainPolyV2 <- getMatriz(twoGramTypesInfoGain, data_test)
    resultados <- addRow(resultados, "2 Gram + Info Gain + Q2 (Poly V2)", matrizTwoGramTypesInfoGainPolyV2)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesCFSQ2EntidadesV2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-cfs-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesCFS <- treinar(data_train)
    twoGramTypesCFS
    matrizTwoGramTypesCFSQ2Entidades <- getMatriz(twoGramTypesCFS, data_test)
    resultados <- addRow(resultados, "2 Gram + Types + entidades + CFS Q2", matrizTwoGramTypesCFSQ2Entidades)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesInfoQ2EntidadesV2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesCFS <- treinar(data_train)
    twoGramTypesCFS
    matrizTwoGramTypesInfoQ2Entidades <- getMatriz(twoGramTypesCFS, data_test)
    resultados <- addRow(resultados, "2 Gram + Types + entidades + Info Gain Q2", matrizTwoGramTypesInfoQ2Entidades)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesInfoQ2EntidadesPolyV2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesCFS <- treinarPoly(data_train)
    twoGramTypesCFS
    matrizTwoGramTypesInfoQ2EntidadesPoly <- getMatriz(twoGramTypesCFS, data_test)
    resultados <- addRow(resultados, "2 Gram + Types + entidades + Info Gain Q2 (Poly)", matrizTwoGramTypesInfoQ2EntidadesPoly)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matrizTwoGramTypesInfoQ2Entidades5V2")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramTypesCFS <- treinarFolds(data_train, 5)
    twoGramTypesCFS
    matrizTwoGramTypesInfoQ2Entidades5 <- getMatriz(twoGramTypesCFS, data_test)
    resultados <- addRow(resultados, "2 Gram + Types + entidades + Info Gain Q2 (5 folds)", matrizTwoGramTypesInfoQ2Entidades5)
    save.image(file="2110/rdas/testes2605.RData")
  })
}

if (!exists("matriz2GramEntidadesHoraErroV2")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro-q2.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramEntidadesHoraErro <- treinar(data_train)
    twogramEntidadesHoraErro
    matriz2GramEntidadesHoraErro <- getMatriz(twogramEntidadesHoraErro, data_test)
    resultados <- addRow(resultados, "2 GRAM + entidades + hora + erro", matriz2GramEntidadesHoraErro)
  })
}

if (!exists("matriz3GramV2")) {
  try({
    load("2110/rdas/3gram-25-q2.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    threeGram25 <- treinar(data_train)
    threeGram25
    matriz3Gram <- getMatriz(threeGram25, data_test)
    resultados <- addRow(resultados, "3 Gram + 25% + Bow #", matriz3Gram)

    load("2110/rdas/3gram-25-q2.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    threeGram25 <- treinarFolds(data_train, 5)
    threeGram25
    matriz3Gram <- getMatriz(threeGram25, data_test)
    resultados <- addRow(resultados, "3 Gram + 25% + Bow # (5)", matriz3Gram)

  })
}

limpar <- function() {
  rm("aspectos","aspectosManter","aspectosRemover","dados","dadosQ1","data_test","data_train","dataFrameEntidades","dataFrameEstabelecimento","dataFrameGram","dataFrameHash","dataFrameTexto","dbpediaTypesCompleto","dbpediaTypesPageRank","dtm_train_hash_tags","dtm_train_texto","maFinal","matriz2Gram","matriz2Gram25","matriz2GramDBPedia","matriz2GramEntidades","matriz3Gram25","matrizDBPediaCompleto","matrizDBPediaTypesPage","matrizThreeGram","matrizTwoGram25Hora","matrizTwoGramCateogoriaLocalizacao","matrizTwoGramDBPediaSubject","matrizTwoGramDiaSemana","matrizTwoGramEntidades2","matrizTwoGramEntidadesHora","matrizTwoGramEntidadesHoraErro","matrizTwoGramErros","matrizWikipedia","threeGram","treegram25","twoGDBPedia","twogram","twogram25","twoGram25Hora","twoGramCategoriaLocalizacao","twoGramDBPediaSubject","twoGramDiaSemana","twogramentidades","twoGramEntidades25","twoGramEntidadesHora","twoGramEntidadesHoraErro", "twoGramErros","vectorizerEstabelecimento","vectorizerHashTags","vocab","vocabEstabelecimento","vocabHashTags","wikipediaCategory")
  save.image(file="2110/rdas/compare22_resultado.RData")
}