library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 5
registerDoMC(CORES)

treinar <- function(data_train){
    fit <- train(x = subset(data_train, select = -c(resposta)),
            y = data_train$resposta, 
            method = "svmLinear", 
            trControl = trainControl(method = "cv", number = 5, savePred=T))
            #trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE))
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

library(magrittr)

set.seed(10)
split=0.80

try({
  load("pre_selecionar/gram-q2-not-null-union-v2.Rda")
  model <- treinar(maFinal)
})

load("pre_selecionar/gram-adaptado-union_test-v2.Rda")
pred <- predict(model, subset(maClassificar, select = -c(idzaoTweet, resposta)))
pred

maPred <- cbind.fill(subset(maClassificar, select = c(idzaoTweet)), pred)
write.csv(maPred, "pre_selecionar/pred_union_v2.csv")