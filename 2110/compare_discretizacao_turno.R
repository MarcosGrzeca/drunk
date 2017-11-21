resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
  load("2110/rdas/compare_turno.RData")
})

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 7
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
  #save.image(file="2110/rdas/compare_turno.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

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
    save.image(file="2110/rdas/compare_turno.RData")
  })
}

if (!exists("matrizTwoGramEntidadesHoraErroModelo1")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro-modelo1.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErroModelo1 <- treinar(data_train)
    twoGramEntidadesHoraErroModelo1
    matrizTwoGramEntidadesHoraErroModelo1 <- getMatriz(twoGramEntidadesHoraErroModelo1, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora Modelo 1 + Erro", matrizTwoGramEntidadesHoraErroModelo1)
    save.image(file="2110/rdas/compare_turno.RData")
  })
}

if (!exists("matrizTwoGramEntidadesHoraErroModelo2")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro-modelo2.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErroModelo2 <- treinar(data_train)
    twoGramEntidadesHoraErroModelo2
    matrizTwoGramEntidadesHoraErroModelo2 <- getMatriz(twoGramEntidadesHoraErroModelo2, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora Modelo 2 + Erro", matrizTwoGramEntidadesHoraErroModelo2)
    save.image(file="2110/rdas/compare_turno.RData")
  })
}