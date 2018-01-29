resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
	load("mardigras/mardigras_compare.RData")
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
            trControl = trainControl(method = "cv", number = 5, savePred=T))
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
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

if (!exists("matriz2GramEntidades")) {
  try({
    load("mardigras/2gram-entidades.Rda")
	pred <- predict(twoGramEntidadesHoraErroNotNull, subset(maFinal, select = -c(resposta)))
  	#matriz <- confusionMatrix(data = pred, data_test$resposta, positive="1")

  })
}

load("mardigras/2gram-entidades.Rda")
nrow(maFinal)

teste <- maFinal

resultados
predNew <- predict(twoGramEntidadesHoraErroNotNull, newData = head(resultados))
predNew <- predict(twoGramEntidadesHoraErroNotNull, maFinal)
predNew
dump(predNew, "mardigras/pred.csv")

bothModels <- list(knn = twoGramEntidadesHoraErroNotNull)

predict(bothModels)

View(extractPrediction(bothModels, maFinal))


load("2110/rdas/2gram-entidades-hora-erro-not-null.Rda")
antigo <- maFinal
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

nrow(data_test)

nrow(antigo)

predict(twoGramEntidadesHoraErroNotNull, subset(maFinal,    select = -c(resposta)))

predict(twoGramEntidadesHoraErroNotNull, subset(data_test, select = -c(resposta)))



load("2110/rdas/2gram-entidades-hora-erro-not-null-new-test.Rda")
nrow(maFinal)
