resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
  load("2110/rdas/compare-baseline-cross5.RData")
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
            trControl = trainControl(method = "cv", number = 5, savePred=T))
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
  #save.image(file="2110/rdas/compare-baseline-cross5.RData")
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
    resultados <- addRow(resultados, "3 GRAM 25", matriz3Gram25)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
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
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("matriz3Gram25Q2")) {
  try({
    load("2110/rdas/3gram-25-q2.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    treegram25Q2 <- treinar(data_train)
    treegram25Q2
    matriz3Gram25Q2 <- getMatriz(treegram25Q2, data_test)
    resultados <- addRow(resultados, "3 GRAM 25 Q2", matriz3Gram25Q2)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("matriz2GramEntidadesHoraErroQ2")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro-q2.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErroQ2 <- treinar(data_train)
    twoGramEntidadesHoraErroQ2
    matriz2GramEntidadesHoraErroQ2 <- getMatriz(twoGramEntidadesHoraErroQ2, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora + Erro Q2", matriz2GramEntidadesHoraErroQ2)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("matriz3Gram25Q3")) {
  try({
    load("2110/rdas/3gram-25-q3.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    treegram25Q3 <- treinar(data_train)
    treegram25Q3
    matriz3Gram25Q3 <- getMatriz(treegram25Q3, data_test)
    resultados <- addRow(resultados, "3 GRAM 25 Q3", matriz3Gram25Q3)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("matriz2GramEntidadesHoraErroQ3")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro-q3.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErroQ3 <- treinar(data_train)
    twoGramEntidadesHoraErroQ3
    matriz2GramEntidadesHoraErroQ3 <- getMatriz(twoGramEntidadesHoraErroQ3, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora + Erro Q3", matriz2GramEntidadesHoraErroQ3)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("matriz3Gram25Q2NotNull")) {
  try({
    load("2110/rdas/3gram-25-q2-not-null.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    treegram25Q2NotNull <- treinar(data_train)
    treegram25Q2NotNull
    matriz3Gram25Q2NotNull <- getMatriz(treegram25Q2NotNull, data_test)
    resultados <- addRow(resultados, "3 GRAM 25 Q2 (Not Null)", matriz3Gram25Q2NotNull)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("twoGramEntidadesHoraErroQ2NotNull")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro-q2-not-null.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErroQ2NotNull <- treinar(data_train)
    twoGramEntidadesHoraErroQ2NotNull
    matriz2GramEntidadesHoraErroQ2NotNull <- getMatriz(twoGramEntidadesHoraErroQ2NotNull, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora + Erro Q2 (Not Null)", matriz2GramEntidadesHoraErroQ2NotNull)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("matriz3Gram25Q3NotNull")) {
  try({
    load("2110/rdas/3gram-25-q3-not-null.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    treegram25Q3NotNull <- treinar(data_train)
    treegram25Q3NotNull
    matriz3Gram25Q3NotNull <- getMatriz(treegram25Q3NotNull, data_test)
    resultados <- addRow(resultados, "3 GRAM 25 Q3 (Not Null)", matriz3Gram25Q3NotNull)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}

if (!exists("matriz2GramEntidadesHoraErroQ3NotNull")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro-q3-not-null.Rda")
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErroQ3NotNull <- treinar(data_train)
    twoGramEntidadesHoraErroQ3NotNull
    matriz2GramEntidadesHoraErroQ3NotNull <- getMatriz(twoGramEntidadesHoraErroQ3NotNull, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora + Erro Q3 (Not Null)", matriz2GramEntidadesHoraErroQ3NotNull)
    save.image(file="2110/rdas/compare-baseline-cross5.RData")
  })
}


print("FIIMMMMMMMMMMMMMMMMMM")