library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

treinar <- function(data_train){
    registerDoMC(16)
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 10, savePred=T),
            preProc=c("center"))
    return (fit)
}

getMatriz <- function(fit, data_test) {
  registerDoMC(16)
  pred <- predict(fit, subset(data_test, select = -c(resposta)))
  matriz <- confusionMatrix(data = pred, data_test$resposta, positive="1")
  return (matriz)
}

resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

addRow <- function(resultados, baseline, matriz, ...) {
  newRes <- data.frame(baseline, matriz$byClass["F1"], matriz$byClass["Precision"], matriz$byClass["Recall"])
  rownames(newRes) <- baseline
  names(newRes) <- c("Baseline", "F1", "Precisão", "Revocação")
  newdf <- rbind(resultados, newRes)
  return (newdf)
}

library(magrittr)


registerDoMC(8)

set.seed(10)
split=0.80

#Baseline 2013

load("baselines/dataset/2013/bof.Rda")
print("2013")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2013bof <- treinar(data_train)
fit2013bof
matrizt2013bof <- getMatriz(fit2013bof, data_test)
resultados <- addRow(resultados, "2013 BOW", matrizt2013bof)

load("baselines/dataset/2013/bof_presence.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2013bofPresence <- treinar(data_train)
fit2013bofPresence
matrizt2013bofPresence <- getMatriz(fit2013bofPresence, data_test)
resultados <- addRow(resultados, "2013 BOW Presence", matrizt2013bofPresence)

print("2014")
load("baselines/dataset/2014/bof_uni.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2014uni <- treinar(data_train)
fit2014uni
matriz2014uni <- getMatriz(fit2014uni, data_test)
resultados <- addRow(resultados, "2014 1-Gram", matriz2014uni)

load("baselines/dataset/2014/bof_bi.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2014bi <- treinar(data_train)
fit2014bi
matriz2014bi <- getMatriz(fit2014bi, data_test)
resultados <- addRow(resultados, "2014 2-Gram", matriz2014bi)

load("baselines/dataset/2014/bof_stem_uni.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2014sun <- treinar(data_train)
fit2014sun
matriz2014suni <- getMatriz(fit2014sun, data_test)
resultados <- addRow(resultados, "2014 Stemming 1-Gram", matriz2014suni)

load("baselines/dataset/2014/bof_stem_bi.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2014sbi <- treinar(data_train)
fit2014sbi
matriz2014sbi <- getMatriz(fit2014sbi, data_test)
resultados <- addRow(resultados, "2014 Stemming 2-Gram", matriz2014sbi)

print("2015")
load("baselines/dataset/2015/bof_uni.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2015un <- treinar(data_train)
fit2015un
matriz2015un <- getMatriz(fit2015un, data_test)
resultados <- addRow(resultados, "2015 1-Gram", matriz2015un)

load("baselines/dataset/2015/bof_bi.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2015bi <- treinar(data_train)
fit2015bi
matriz2015bi <- getMatriz(fit2015bi, data_test)
resultados <- addRow(resultados, "2015 2-Gram", matriz2015bi)

load("baselines/dataset/2015/bof_uni_presence.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2015unPresence <- treinar(data_train)
fit2015unPresence
matriz2015unPresence <- getMatriz(fit2015unPresence, data_test)
resultados <- addRow(resultados, "2015 1-Gram Presence", matriz2015unPresence)

load("baselines/dataset/2015/bof_bi_presence.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2015biPresence <- treinar(data_train)
fit2015biPresence
matriz2015biPresence <- getMatriz(fit2015biPresence, data_test)
resultados <- addRow(resultados, "2015 2-Gram Presence", matriz2015biPresence)

load("baselines/dataset/2015/stile_gram.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2015Style1 <- treinar(data_train)
fit2015Style1
matriz2015Style1 <- getMatriz(fit2015Style1, data_test)
resultados <- addRow(resultados, "2015 Style", matriz2015Style1)

load("baselines/dataset/2015/stile_gram_presence.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

fit2015Style2 <- treinar(data_train)
fit2015Style2
matriz2015Style2 <- getMatriz(fit2015Style2, data_test)
resultados <- addRow(resultados, "2015 Style Presence", matriz2015Style2)


print("2016")
load("baselines/dataset/2016/2016.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]
fit2016 <- treinar(data_train)
fit2016
matriz2016 <- getMatriz(fit2016, data_test)
resultados <- addRow(resultados, "2016", matriz2016)

save.image(file="baselines/compare.RData")
resultados