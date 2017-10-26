library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

treinar <- function(data_train){
    registerDoMC(10)
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 10, savePred=T),
            preProc=c("center"))
    return (fit)
}

getMatriz <- function(fit, data_test) {
  registerDoMC(10)
  pred <- predict(fit, subset(data_test, select = -c(resposta)))
  matriz <- confusionMatrix(data = pred, data_test$resposta, positive="1")
  return (matriz)
}

resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

addRow <- function(resultados, baseline, matriz, ...) {
  print(baseline)
  newRes <- data.frame(baseline, matriz$byClass["F1"], matriz$byClass["Precision"], matriz$byClass["Recall"])
  rownames(newRes) <- baseline
  names(newRes) <- c("Baseline", "F1", "Precisão", "Revocação")
  newdf <- rbind(resultados, newRes)
  save.image(file="2110/rdas/compare21.RData")
  return (newdf)
}

library(magrittr)


registerDoMC(10)

set.seed(10)
split=0.80

#Baseline 2013

load("2110/rdas/2gram.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twogram <- treinar(data_train)
twogram
matriz2Gram <- getMatriz(twogram, data_test)
resultados <- addRow(resultados, "2Gram", matriz2Gram)

load("2110/rdas/2gram-25.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twogram25 <- treinar(data_train)
twogram25
matriz2Gram25 <- getMatriz(twogram25, data_test)
resultados <- addRow(resultados, "2GRAM 25%", matriz2Gram25)

load("2110/rdas/2gram-entidades.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twogramentidades <- treinar(data_train)
twogramentidades
matriz2GramEntidades <- getMatriz(twogramentidades, data_test)
resultados <- addRow(resultados, "2GRAM entidades", matriz2GramEntidades)

load("2110/rdas/3gram-25.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

treegram25 <- treinar(data_train)
treegram25
matriz3Gram25 <- getMatriz(treegram25, data_test)
resultados <- addRow(resultados, "3GRAM 25", matriz3Gram25)

load("2110/rdas/2gram-dbpedia.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twoGDBPedia <- treinar(data_train)
twoGDBPedia
matriz2GramDBPedia <- getMatriz(twoGDBPedia, data_test)
resultados <- addRow(resultados, "2 Gram DBPedia Resource", matriz2GramDBPedia)

load("2110/rdas/2gram-wikipedia.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

wikipediaCategory <- treinar(data_train)
wikipediaCategory
matrizWikipedia <- getMatriz(wikipediaCategory, data_test)
resultados <- addRow(resultados, "2 Gram Wikipedia", matrizWikipedia)

load("2110/rdas/2-Gram-dbpedia-types-page-rank.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

dbpediaTypesPageRank <- treinar(data_train)
dbpediaTypesPageRank
matrizDBPediaTypesPage <- getMatriz(dbpediaTypesPageRank, data_test)
resultados <- addRow(resultados, "2 Gram dbpedia types page rank", matrizDBPediaTypesPage)

load("2110/rdas/2-Gram-dbpedia-types-completo.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

dbpediaTypesCompleto <- treinar(data_train)
dbpediaTypesCompleto
matrizDBPediaCompleto <- getMatriz(dbpediaTypesCompleto, data_test)
resultados <- addRow(resultados, "2 Gram dbpedia types (todos)", matrizDBPediaCompleto)

load("2110/rdas/2gram-entidades-25.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twoGramEntidades25 <- treinar(data_train)
twoGramEntidades25
matrizTwoGramEntidades2 <- getMatriz(twoGramEntidades25, data_test)
resultados <- addRow(resultados, "2 Gram 25% + entidades", matrizTwoGramEntidades2)

load("2110/rdas/2-Gram-dbpedia-subject.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twoGramDBPediaSubject <- treinar(data_train)
twoGramDBPediaSubject
matrizTwoGramDBPediaSubject <- getMatriz(twoGramDBPediaSubject, data_test)
resultados <- addRow(resultados, "2 Gram + subject", matrizTwoGramDBPediaSubject)

save.image(file="2110/rdas/compare22.RData")

#load("2110/rdas/compare21.RData")
#View(resultados)