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

library(magrittr)

set.seed(10)
split=0.80

try({
	load("2110/rdas/2gram-q2-not-null.Rda")
	model <- treinarPoly(maFinal)
	#saveRDS(model, "pre_selecionar/model_q2.rds")
    #save(model, file="pre_selecionar/model_q2.Rdata")
    save.image("pre_selecionar/train_local.Rdata")

})