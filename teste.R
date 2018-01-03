resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Técnica", "F1", "Precisão", "Revocação")

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <-10
registerDoMC(CORES)

treinar <- function(data_train){
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 5, savePred=T)
            ,preProc=c("center")
            )
    return (fit)
}

treinarSemCenter <- function(data_train){
    # registerDoMC(CORES)


    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 10, savePred=T)
            )
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


load("2110/rdas/2gram-entidades-alchemy-categories-not-null-resposta.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
#levels(maFinal$resposta) <- make.names(levels(factor(maFinal$resposta)))

trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twogramAlchemyCategoriesNotNull <- treinar(data_train)
twogramAlchemyCategoriesNotNull
matriz2AlchemyCategoriesNotNull <- getMatriz(twogramAlchemyCategoriesNotNull, data_test)
resultados <- addRow(resultados, "TESTE", matriz2AlchemyCategoriesNotNull)
  

print("FIMMMMMMMMMMmm")