resultados3 <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados3) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
    load("webintelligence/compare_estatistico_q3.RData")
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

if (!exists("matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPoly")) {
  try({
    for (indice in 1:10){
      load("2110/rdas/2-Gram-dbpedia-types-enriquecimento-info-q3-not-null_info_entidades.Rda")
      maFinal$resposta <- as.factor(maFinal$resposta)
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      twoGramTypesCFS <- treinarPoly(data_train)
      twoGramTypesCFS
      matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPoly <- getMatriz(twoGramTypesCFS, data_test)
      resultados3 <- addRow(resultados3, "2 Gram + Types (Info Gain) + Entidades (Info Gain) + Q3 (Poly)", matrizTwoGramTypesInfoQ2EntidadesEnriquecimentoEPodaPoly)
      save.image(file="webintelligence/compare_estatistico_q3.RData")
    }
  })
}

if (!exists("matriz3Gram25Q3NotNull")) {
  try({
    for (indice in 1:10){
      load("2110/rdas/3gram-25-q3-not-null.Rda")
      trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
      data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
      data_test <- maFinal[-trainIndex,]

      treegram25Q3NotNull <- treinar(data_train)
      treegram25Q3NotNull
      matriz3Gram25Q3NotNull <- getMatriz(treegram25Q3NotNull, data_test)
      resultados3 <- addRow(resultados3, "3 GRAM 25 Q3 (Not Null)", matriz3Gram25Q3NotNull)
      save.image(file="webintelligence/compare_estatistico_q3.RData")
    }
  })
}