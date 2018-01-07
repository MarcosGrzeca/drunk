resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

try({
  load("2110/rdas/compare_artigo_exaustivo.RData")
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
  #save.image(file="2110/rdas/compare_artigo_exaustivo.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

if (!exists("matriz2GramDBPedia")) {
  #FEITO
  try({
    load("2110/rdas/2gram-dbpedia-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGDBPedia <- treinar(data_train)
    twoGDBPedia
    matriz2GramDBPedia <- getMatriz(twoGDBPedia, data_test)
    resultados <- addRow(resultados, "2 Gram DBPedia Resource", matriz2GramDBPedia)
    save.image(file="2110/rdas/compare_artigo_exaustivo.RData")
  })
}

if (!exists("matrizWikipedia")) {
  #FEITO
  load("2110/rdas/2gram-wikipedia-not-null.Rda")
  maFinal$resposta <- as.factor(maFinal$resposta)
  trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
  data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
  data_test <- maFinal[-trainIndex,]

  wikipediaCategory <- treinar(data_train)
  wikipediaCategory
  matrizWikipedia <- getMatriz(wikipediaCategory, data_test)
  resultados <- addRow(resultados, "2 Gram Wikipedia", matrizWikipedia)
  save.image(file="2110/rdas/compare_artigo_exaustivo.RData")
}

if (!exists("matrizDBPediaTypesPage")) {
  #FEITO
  try({
    load("2110/rdas/2-Gram-dbpedia-types-page-rank-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    dbpediaTypesPageRank <- treinar(data_train)
    dbpediaTypesPageRank
    matrizDBPediaTypesPage <- getMatriz(dbpediaTypesPageRank, data_test)
    resultados <- addRow(resultados, "2 Gram dbpedia types page rank", matrizDBPediaTypesPage)
    save.image(file="2110/rdas/compare_artigo_exaustivo.RData")
  })
}

if (!exists("matrizDBPediaCompleto")) {
  #FEITO
  try({
    load("2110/rdas/2-Gram-dbpedia-types-completo-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    dbpediaTypesCompleto <- treinar(data_train)
    dbpediaTypesCompleto
    matrizDBPediaCompleto <- getMatriz(dbpediaTypesCompleto, data_test)
    resultados <- addRow(resultados, "2 Gram dbpedia types (todos)", matrizDBPediaCompleto)
    save.image(file="2110/rdas/compare_artigo_exaustivo.RData")
  })
}

if (!exists("matrizTwoGramDBPediaSubject")) {
  #FEITO
  try({
    load("2110/rdas/2-Gram-dbpedia-subject-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramDBPediaSubject <- treinar(data_train)
    twoGramDBPediaSubject
    matrizTwoGramDBPediaSubject <- getMatriz(twoGramDBPediaSubject, data_test)
    resultados <- addRow(resultados, "2 Gram + subject", matrizTwoGramDBPediaSubject)
    save.image(file="2110/rdas/compare_artigo_exaustivo.RData")
  })
}

if (!exists("matrizTwoGramCateogoriaLocalizacao")) {
  try({
    load("2110/rdas/2gram-25-categoria-localizacao-not-null.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramCategoriaLocalizacao <- treinar(data_train)
    twoGramCategoriaLocalizacao
    matrizTwoGramCateogoriaLocalizacao <- getMatriz(twoGramCategoriaLocalizacao, data_test)
    resultados <- addRow(resultados, "2 Gram + 25% + Categoria da localização", matrizTwoGramCateogoriaLocalizacao)
    save.image(file="2110/rdas/compare_artigo_exaustivo.RData")
  })
}

print("FIIMMMMMMMMMMMMMMMMMM")

#load("2110/rdas/compare_artigo_exaustivo.RData")
#resultados
#View(resultados)

limpar()

limpar <- function() {
  rm("aspectos","aspectosManter","aspectosRemover","dados","dadosQ1","data_test","data_train","dataFrameEntidades","dataFrameEstabelecimento","dataFrameGram","dataFrameHash","dataFrameTexto","dbpediaTypesCompleto","dbpediaTypesPageRank","dtm_train_hash_tags","dtm_train_texto","maFinal","matriz2Gram","matriz2Gram25","matriz2GramDBPedia","matriz2GramEntidades","matriz3Gram25","matrizDBPediaCompleto","matrizDBPediaTypesPage","matrizThreeGram","matrizTwoGram25Hora","matrizTwoGramCateogoriaLocalizacao","matrizTwoGramDBPediaSubject","matrizTwoGramDiaSemana","matrizTwoGramEntidades2","matrizTwoGramEntidadesHora","matrizTwoGramEntidadesHoraErro","matrizTwoGramErros","matrizWikipedia","threeGram","treegram25","twoGDBPedia","twogram","twogram25","twoGram25Hora","twoGramCategoriaLocalizacao","twoGramDBPediaSubject","twoGramDiaSemana","twogramentidades","twoGramEntidades25","twoGramEntidadesHora","twoGramEntidadesHoraErro", "twoGramErros","vectorizerEstabelecimento","vectorizerHashTags","vocab","vocabEstabelecimento","vocabHashTags","wikipediaCategory")
  save.image(file="2110/rdas/compare22_resultado.RData")
}
