load("pre_selecionar/tmp.Rdata")
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
            trControl = trainControl(method = "cv", number = 5, savePred = TRUE, allowParallel = TRUE))
            #trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE))
    return (fit)
}

treinarProb <- function(data_train, setY){
  fit <- train(x = data_train,
               y = setY, 
               method = "svmLinear", 
               trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE, savePred = TRUE, allowParallel = TRUE))
  return (fit)
}

library(magrittr)

set.seed(10)
split=0.80

try({
	load("2110/rdas/2gram-q2-not-null.Rda")
	model <- treinar(maFinal)

	data_train <- subset(maFinal, select = -c(resposta))
	#data_train_names <- data_train
	#make.names(data_train_names)
	
	require(dplyr)
	maFinalBkp <- maFinalBkp %>%
	          mutate(resposta = ifelse(resposta == 1,"Yes", "No"))
	
	modelProb <- treinarProb(data_train, maFinalBkp$resposta)
	save.image("pre_selecionar/tmp.Rdata")
})

trainData <- subset(maFinal, select = -c(resposta))

#load("pre_selecionar/2gram-candidatos.Rda");
str(maFinal)

pred <- predict(model, newdata = data_train ,type="prob")
View(pred)
str(maFinal$resposta)

matriz <- confusionMatrix(data = pred, maFinal$resposta, positive="Yes")


str(pred)
pred

#maFinal$randomm[maFinal$randomm > 1] <- 0

##ADICIONAIS
#make.names(maFinal)