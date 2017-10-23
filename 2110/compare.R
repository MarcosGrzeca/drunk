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
  save.image(file="2110/compare21.RData")
  return (newdf)
}

library(magrittr)


registerDoMC(10)

set.seed(10)
split=0.80

#Baseline 2013

load("2110/2gram.Rda")
print("2GRAM")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twogram <- treinar(data_train)
twogram
matriz2Gram <- getMatriz(twogram, data_test)
resultados <- addRow(resultados, "2Gram", matriz2Gram)

load("2110/2gram-25.Rda")
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twogram25 <- treinar(data_train)
twogram25
matriz2Gram25 <- getMatriz(twogram25, data_test)
resultados <- addRow(resultados, "2GRAM 25%", matriz2Gram25)
save.image(file="2110/compare21.RData")