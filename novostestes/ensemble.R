resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

library(tools)
library(caret)
if (!require("caretEnsemble")) {
  install.packages("caretEnsemble")
}
library(caretEnsemble)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 25
registerDoMC(CORES)

treinar <- function(data_train){
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 5, savePred=T),
            preProc=c("center"))
    return (fit)
}

treinarEnsemble <- function(data_train) {
    fit <- train( = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 5, savePred=T),
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
  #save.image(file="webintelligence/compare.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

# try({
  # for (indice in 1:3){
    load("2110/rdas/2gram-entidades-hora-erro-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    dataset = 
    control <- trainControl(method="repeatedcv", number=5, repeats=3, savePredictions=TRUE)
    #algorithmList <- c('lda', 'rpart', 'rf', 'knn', 'svmRadial')
    algorithmList <- c('svmLinear', 'rf')
    models <- caretList(y = data_train$resposta, x=subset(data_train, select = -c(resposta)), trControl=control, methodList=algorithmList)
    results <- resamples(models)


    twogramEntidadesHoraErro <- treinar(data_train)
    twogramEntidadesHoraErro
    matriz2GramEntidadesHoraErro <- getMatriz(twogramEntidadesHoraErro, data_test)
    resultados <- addRow(resultados, "2 GRAM + entidades + hora + erro", matriz2GramEntidadesHoraErro)
    #save.image(file="webintelligence/compare.RData")
  # }
# })

try({
  for (indice in 1:3){
    load("2110/rdas/2gram-entidades-hora-erro-q2-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramEntidadesHoraErro <- treinar(data_train)
    twogramEntidadesHoraErro
    matriz2GramEntidadesHoraErro <- getMatriz(twogramEntidadesHoraErro, data_test)
    resultados <- addRow(resultados, "2 GRAM + entidades + hora + erro", matriz2GramEntidadesHoraErro)
    #save.image(file="webintelligence/compare.RData")
  }
})
