library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 2
registerDoMC(CORES)

treinar <- function(data_train){
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 5, savePred=T))
    return (fit)
}

treinarPoly <- function(data_train){
    # registerDoMC(CORES)
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmPoly", 
            trControl = trainControl(method = "cv", number = 5, savePred=T))
    return (fit)
}

getMatriz <- function(fit, data_test) {
  # registerDoMC(CORES)
  pred <- predict(fit, subset(data_test, select = -c(resposta)))
  matriz <- confusionMatrix(data = pred, data_test$resposta, positive="1")
  return (matriz)
}

library(magrittr)

set.seed(10)
split = 0.80

#try({
	 model1 <- readRDS("pre_selecionar/model_q2.rds")
   load("pre_selecionar/model_q2.Rdata")
   


   load("pre_selecionar/2gram-candidatos.Rda");
	 data_test <- subset(maFinal, select = -c(idzaoTweet))
   data_test <- subset(maFinal, select = -c(resposta))
   pred <- predict(model, data_test)
#})

load("pre_selecionar/train.Rdata")
   