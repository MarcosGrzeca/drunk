resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

load("2110/rdas/compare-baseline.RData")

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
  #save.image(file="2110/rdas/compare-baseline.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

if (!exists("matriz3Gram25")) {
  try({
    load("2110/rdas/3gram-25-teste.Rda")
    #maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    treegram25 <- treinar(data_train)
    treegram25
    matriz3Gram25 <- getMatriz(treegram25, data_test)
    resultados <- addRow(resultados, "3GRAM 25", matriz3Gram25)
    save.image(file="2110/rdas/compare-baseline.RData")
  })
}

if (!exists("matrizTwoGramEntidadesHoraErro")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErro <- treinar(data_train)
    twoGramEntidadesHoraErro
    matrizTwoGramEntidadesHoraErro <- getMatriz(twoGramEntidadesHoraErro, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora + Erro", matrizTwoGramEntidadesHoraErro)
    save.image(file="2110/rdas/compare-baseline.RData")
  })
}
print("FIIMMMMMMMMMMMMMMMMMM")