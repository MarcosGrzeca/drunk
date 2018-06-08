resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
    load("webintelligence/compareq1_v30.RData")
})

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 5
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
  #save.image(file="webintelligence/compare_estatistico_0406.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

if (!exists("matriz3Gram25NotNull")) {
  try({
    for (indice in 1:20){ 
      load("2110/rdas/3gram-25-not-null.Rda")
      maFinal$resposta <- as.factor(maFinal$resposta)
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      treegram25NotNull <- treinar(data_train)
      treegram25NotNull
      matriz3Gram25NotNull <- getMatriz(treegram25NotNull, data_test)
      resultados <- addRow(resultados, "(Q1) 3 GRAM 25 Not Null", matriz3Gram25NotNull)
      save.image(file="webintelligence/compareq1_v30.RData")
    }
  })
}

if (!exists("matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyQ1")) {
  try({
    for (indice in 1:20){
      load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q1-not-null_info_entidades.Rda")
      maFinal$resposta <- as.factor(maFinal$resposta)
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      twoGramTypesCFSQ1 <- treinarPoly(data_train)
      twoGramTypesCFSQ1
      matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyQ1 <- getMatriz(twoGramTypesCFSQ1, data_test)
      resultados <- addRow(resultados, "(Q1) 2 Gram + Types (Info Gain) + Entidades (Info Gain) + Q1 (Poly)", matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyQ1)
      save.image(file="webintelligence/compareq1_v30.RData")
    }
  })
}

if (!exists("matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyQ1V2")) {
  try({
    for (indice in 1:17){
      load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q1-not-null_info_entidades.Rda")
      maFinal$resposta <- as.factor(maFinal$resposta)
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      twoGramTypesCFSQ1 <- treinarPoly(data_train)
      twoGramTypesCFSQ1
      matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyQ1V2 <- getMatriz(twoGramTypesCFSQ1, data_test)
      resultados <- addRow(resultados, "(Q1) 2 Gram + Types (Info Gain) + Entidades (Info Gain) + Q1 (Poly)", matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPolyQ1V2)
      save.image(file="webintelligence/compareq1_v30.RData")
    }
  })
}