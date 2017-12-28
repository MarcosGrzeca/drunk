resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Técnica", "F1", "Precisão", "Revocação")

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 10
registerDoMC(CORES)

load("2110/rdas/compare_entities.RData")

saveImg <- function() {
  save.image(file="2110/rdas/compare_entities.RData")
}

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
  names(newRes) <- c("Técnica", "F1", "Precisão", "Revocação")
  newdf <- rbind(resultados, newRes)
  #saveImg()
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

if (!exists("matriz2Calais")) {
  try({
    load("2110/rdas/2gram-entidades-calais.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramCalais <- treinar(data_train)
    twogramCalais
    matriz2Calais <- getMatriz(twogramCalais, data_test)
    resultados <- addRow(resultados, "Calais", matriz2Calais)
    saveImg()
  })
}

if (!exists("matriz2AlchemyEntities")) {
  try({
    load("2110/rdas/2gram-entidades-alchemy-entities.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramAlchemyEntities <- treinar(data_train)
    twogramAlchemyEntities
    matriz2AlchemyEntities <- getMatriz(twogramAlchemyEntities, data_test)
    resultados <- addRow(resultados, "Alchemy Entities", matriz2AlchemyEntities)
    saveImg()
  })
}

if (!exists("matriz2AlchemyKeywords")) {
  try({
    load("2110/rdas/2gram-entidades-alchemy-keywords.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramAlchemyKeywords <- treinar(data_train)
    twogramAlchemyKeywords
    matriz2AlchemyKeywords <- getMatriz(twogramAlchemyKeywords, data_test)
    resultados <- addRow(resultados, "Alchemy KeyWords", matriz2AlchemyKeywords)
    saveImg()
  })
}

if (!exists("matriz2AlchemyConcepts")) {
  try({
    load("2110/rdas/2gram-entidades-alchemy-concepts.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    maFinal <- maFinal[, !duplicated(colnames(maFinal))]
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    #twogramAlchemyConcepts <- treinar(data_train)
    #twogramAlchemyConcepts
    #matriz2AlchemyConcepts <- getMatriz(twogramAlchemyConcepts, data_test)
    #resultados <- addRow(resultados, "Alchemy Concepts", matriz2AlchemyConcepts)
    #saveImg()
  })
}

if (!exists("matriz2AlchemyCategories")) {
  try({
    load("2110/rdas/2gram-entidades-alchemy-categories.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramAlchemyCategories <- treinar(data_train)
    twogramAlchemyCategories
    matriz2AlchemyCategories <- getMatriz(twogramAlchemyCategories, data_test)
    resultados <- addRow(resultados, "Alchemy Categories", matriz2AlchemyCategories)
    saveImg()
  })
}

if (!exists("matriz2GramEntidades")) {
  try({
    load("2110/rdas/2gram-entidades.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramentidades <- treinar(data_train)
    twogramentidades
    matriz2GramEntidades <- getMatriz(twogramentidades, data_test)
    resultados <- addRow(resultados, "2GRAM entidades Todas", matriz2GramEntidades)
    saveImg()
  })
}

if (!exists("matriz2GramCategoriesHoraErro")) {
  try({
    load("2110/rdas/2gram-entidades-alchemy-categories-hora-erro.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramCategoriesHoraErro <- treinar(data_train)
    twogramCategoriesHoraErro
    matriz2GramCategoriesHoraErro <- getMatriz(twogramCategoriesHoraErro, data_test)
    resultados <- addRow(resultados, "2GRAM Categorias + Hora + Erro ", matriz2GramCategoriesHoraErro)
    saveImg()
  })
}
print("FIIMMMMMMMMMMMMMMMMMM")