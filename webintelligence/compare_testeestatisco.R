resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
    load("webintelligence/compare_estatistico.RData")
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
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 5, savePred=T),
            preProc=c("center"))
    return (fit)
}

treinarFolds <- function(data_train, numFolds){
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = numFolds, savePred=T),
            preProc=c("center"))
    return (fit)
}

treinarPoly <- function(data_train){
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmPoly", 
            trControl = trainControl(method = "cv", number = 5, savePred=T),
            preProc=c("center"))
    return (fit)
}

getMatriz <- function(fit, data_test) {
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
  #save.image(file="webintelligence/compare_estatistico.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

if (!exists("matriz3Gram")) {
  try({
    for (indice in 1:5) {
      load("2110/rdas/3gram-25-q2-v2-not-null.Rda")
      maFinal$resposta <- as.factor(maFinal$resposta)
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      threeGram25 <- treinar(data_train)
      threeGram25
      matriz3Gram <- getMatriz(threeGram25, data_test)
      resultados <- addRow(resultados, "3 Gram + 25% + Bow #", matriz3Gram)
      save.image(file="webintelligence/compare_estatistico.RData")
      }
  })
}

if (!exists("matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPoly")) {
  try({
    for (indice in 1:5){
      load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q2-not-null_info_entidades.Rda")
      maFinal$resposta <- as.factor(maFinal$resposta)
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      twoGramTypesCFS <- treinarPoly(data_train)
      twoGramTypesCFS
      matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPoly <- getMatriz(twoGramTypesCFS, data_test)
      resultados <- addRow(resultados, "2 Gram + Types (Info Gain) + Entidades (Info Gain) + Q2 (Poly)", matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPoly)
      save.image(file="webintelligence/compare_estatistico.RData")
    }
  })
}

if (!exists("matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyV2")) {
  try({
    for (indice in 1:5){
      load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q2-not-null_info_entidades.Rda")
      maFinal$resposta <- as.factor(maFinal$resposta)
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      twoGramTypesCFSV2 <- treinarPoly(data_train)
      twoGramTypesCFSV2
      matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyV2 <- getMatriz(twoGramTypesCFSV2, data_test)
      resultados <- addRow(resultados, "V2 2 Gram + Types (Info Gain) + Entidades (Info Gain) + Q2 (Poly)", matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyV2)
      save.image(file="webintelligence/compare_estatistico.RData")
    }
  })
}