resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

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
  names(newRes) <- c("Baseline", "F1", "Precisão", "Revocação")
  newdf <- rbind(resultados, newRes)
  #save.image(file="2110/rdas/compare_entities.RData")
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
    save.image(file="2110/rdas/compare_entities.RData")
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
    save.image(file="2110/rdas/compare_entities.RData")
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
    save.image(file="2110/rdas/compare_entities.RData")
  })
}

if (!exists("matriz2AlchemyConcepts")) {
  try({
    load("2110/rdas/2gram-entidades-alchemy-concepts.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramAlchemyConcepts <- treinar(data_train)
    twogramAlchemyConcepts
    matriz2AlchemyConcepts <- getMatriz(twogramAlchemyConcepts, data_test)
    resultados <- addRow(resultados, "Alchemy Concepts", matriz2AlchemyConcepts)
    save.image(file="2110/rdas/compare_entities.RData")
  })
}

print("FIIMMMMMMMMMMMMMMMMMM")
limpar <- function() {
  rm("aspectos","aspectosManter","aspectosRemover","dados","dadosQ1","data_test","data_train","dataFrameEntidades","dataFrameEstabelecimento","dataFrameGram","dataFrameHash","dataFrameTexto","dbpediaTypesCompleto","dbpediaTypesPageRank","dtm_train_hash_tags","dtm_train_texto","maFinal","matriz2Gram","matriz2Gram25","matriz2GramDBPedia","matriz2GramEntidades","matriz3Gram25","matrizDBPediaCompleto","matrizDBPediaTypesPage","matrizThreeGram","matrizTwoGram25Hora","matrizTwoGramCateogoriaLocalizacao","matrizTwoGramDBPediaSubject","matrizTwoGramDiaSemana","matrizTwoGramEntidades2","matrizTwoGramEntidadesHora","matrizTwoGramEntidadesHoraErro","matrizTwoGramErros","matrizWikipedia","threeGram","treegram25","twoGDBPedia","twogram","twogram25","twoGram25Hora","twoGramCategoriaLocalizacao","twoGramDBPediaSubject","twoGramDiaSemana","twogramentidades","twoGramEntidades25","twoGramEntidadesHora","twoGramEntidadesHoraErro", "twoGramErros","vectorizerEstabelecimento","vectorizerHashTags","vocab","vocabEstabelecimento","vocabHashTags","wikipediaCategory")
  save.image(file="2110/rdas/compare22_resultado.RData")
}