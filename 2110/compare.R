resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

load("2110/rdas/compare22.RData")

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
  #save.image(file="2110/rdas/compare22.RData")
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80

if (!exists("matriz2Gram")) {
  try({
    load("2110/rdas/2gram.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogram <- treinar(data_train)
    twogram
    matriz2Gram <- getMatriz(twogram, data_test)
    resultados <- addRow(resultados, "2Gram", matriz2Gram)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matriz2Gram25")) {
  try({
    load("2110/rdas/2gram-25.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogram25 <- treinar(data_train)
    twogram25
    matriz2Gram25 <- getMatriz(twogram25, data_test)
    resultados <- addRow(resultados, "2GRAM 25%", matriz2Gram25)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matriz2GramEntidades")) {
  try({
    load("2110/rdas/2gram-entidades.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twogramentidades <- treinar(data_train)
    twogramentidades
    matriz2GramEntidades <- getMatriz(twogramentidades, data_test)
    resultados <- addRow(resultados, "2GRAM entidades", matriz2GramEntidades)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizThreeGram")) {
  try({
    load("2110/rdas/3gram.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    threeGram <- treinar(data_train)
    threeGram
    matrizThreeGram <- getMatriz(threeGram, data_test)
    resultados <- addRow(resultados, "3 Gram", matrizThreeGram)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matriz3Gram25")) {
  try({
    load("2110/rdas/3gram-25-teste.Rda")
    #maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    treegram25 <- treinar(data_train)
    treegram25
    matriz3Gram25 <- getMatriz(treegram25, data_test)
    resultados <- addRow(resultados, "3GRAM 25", matriz3Gram25)
    save.image(file="2110/rdas/compare22.RData")
  })
}
if (!exists("matriz2GramDBPedia")) {
  try({
    load("2110/rdas/2gram-dbpedia.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGDBPedia <- treinar(data_train)
    twoGDBPedia
    matriz2GramDBPedia <- getMatriz(twoGDBPedia, data_test)
    resultados <- addRow(resultados, "2 Gram DBPedia Resource", matriz2GramDBPedia)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizWikipedia")) {
  load("2110/rdas/2gram-wikipedia.Rda")
  maFinal$resposta <- as.factor(maFinal$resposta)
  trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
  data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
  data_test <- maFinal[-trainIndex,]

  wikipediaCategory <- treinar(data_train)
  wikipediaCategory
  matrizWikipedia <- getMatriz(wikipediaCategory, data_test)
  resultados <- addRow(resultados, "2 Gram Wikipedia", matrizWikipedia)
  save.image(file="2110/rdas/compare22.RData")
}

if (!exists("matrizDBPediaTypesPage")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-page-rank.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    dbpediaTypesPageRank <- treinar(data_train)
    dbpediaTypesPageRank
    matrizDBPediaTypesPage <- getMatriz(dbpediaTypesPageRank, data_test)
    resultados <- addRow(resultados, "2 Gram dbpedia types page rank", matrizDBPediaTypesPage)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizDBPediaCompleto")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-types-completo.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    dbpediaTypesCompleto <- treinar(data_train)
    dbpediaTypesCompleto
    matrizDBPediaCompleto <- getMatriz(dbpediaTypesCompleto, data_test)
    resultados <- addRow(resultados, "2 Gram dbpedia types (todos)", matrizDBPediaCompleto)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramEntidades2")) {
  try({
    load("2110/rdas/2gram-entidades-25.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidades25 <- treinar(data_train)
    twoGramEntidades25
    matrizTwoGramEntidades2 <- getMatriz(twoGramEntidades25, data_test)
    resultados <- addRow(resultados, "2 Gram 25% + entidades", matrizTwoGramEntidades2)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramDBPediaSubject")) {
  try({
    load("2110/rdas/2-Gram-dbpedia-subject.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramDBPediaSubject <- treinar(data_train)
    twoGramDBPediaSubject
    matrizTwoGramDBPediaSubject <- getMatriz(twoGramDBPediaSubject, data_test)
    resultados <- addRow(resultados, "2 Gram + subject", matrizTwoGramDBPediaSubject)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGram25Hora")) {
  try({
    load("2110/rdas/2gram-25-hora.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGram25Hora <- treinar(data_train)
    twoGram25Hora
    matrizTwoGram25Hora <- getMatriz(twoGram25Hora, data_test)
    resultados <- addRow(resultados, "2 Gram + 25% + Hora", matrizTwoGram25Hora)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramCateogoriaLocalizacao")) {
  try({
    load("2110/rdas/2gram-25-categoria-localizacao.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramCategoriaLocalizacao <- treinar(data_train)
    twoGramCategoriaLocalizacao
    matrizTwoGramCateogoriaLocalizacao <- getMatriz(twoGramCategoriaLocalizacao, data_test)
    resultados <- addRow(resultados, "2 Gram + 25% + Categoria da localização", matrizTwoGramCateogoriaLocalizacao)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramEntidadesHora")) {
  try({
    load("2110/rdas/2gram-entidades-hora.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHora <- treinar(data_train)
    twoGramEntidadesHora
    matrizTwoGramEntidadesHora <- getMatriz(twoGramEntidadesHora, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora", matrizTwoGramEntidadesHora)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramErros")) {
  try({
    load("2110/rdas/2gram-erros.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramErros <- treinar(data_train)
    twoGramErros
    matrizTwoGramErros <- getMatriz(twoGramErros, data_test)
    resultados <- addRow(resultados, "2 Gram + Erros", matrizTwoGramErros)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramDiaSemana")) {
  try({
    load("2110/rdas/2gram-dia-semana.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramDiaSemana <- treinar(data_train)
    twoGramDiaSemana
    matrizTwoGramDiaSemana <- getMatriz(twoGramDiaSemana, data_test)
    resultados <- addRow(resultados, "2 Gram + Dia Semana", matrizTwoGramDiaSemana)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramEntidadesHoraErro")) {
  try({
    load("2110/rdas/2gram-entidades-hora-erro.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramEntidadesHoraErro <- treinar(data_train)
    twoGramEntidadesHoraErro
    matrizTwoGramEntidadesHoraErro <- getMatriz(twoGramEntidadesHoraErro, data_test)
    resultados <- addRow(resultados, "2 Gram + Entidades + Hora + Erro", matrizTwoGramEntidadesHoraErro)
    save.image(file="2110/rdas/compare22.RData")
  })
}

if (!exists("matrizTwoGramPalavroes")) {
  try({
    load("2110/rdas/2gram-palavroes.Rda")
    maFinal$resposta <- as.factor(maFinal$resposta)
    trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
    data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
    data_test <- maFinal[-trainIndex,]

    twoGramPalavroes <- treinar(data_train)
    twoGramPalavroes
    matrizTwoGramPalavroes <- getMatriz(twoGramPalavroes, data_test)
    resultados <- addRow(resultados, "2 Gram + Palavroes", matrizTwoGramPalavroes)
    save.image(file="2110/rdas/compare22.RData")
  })
}

print("FIIMMMMMMMMMMMMMMMMMM")

#load("2110/rdas/compare22.RData")
#resultados
#View(resultados)

limpar <- function() {
  rm("aspectos","aspectosManter","aspectosRemover","dados","dadosQ1","data_test","data_train","dataFrameEntidades","dataFrameEstabelecimento","dataFrameGram","dataFrameHash","dataFrameTexto","dbpediaTypesCompleto","dbpediaTypesPageRank","dtm_train_hash_tags","dtm_train_texto","maFinal","matriz2Gram","matriz2Gram25","matriz2GramDBPedia","matriz2GramEntidades","matriz3Gram25","matrizDBPediaCompleto","matrizDBPediaTypesPage","matrizThreeGram","matrizTwoGram25Hora","matrizTwoGramCateogoriaLocalizacao","matrizTwoGramDBPediaSubject","matrizTwoGramDiaSemana","matrizTwoGramEntidades2","matrizTwoGramEntidadesHora","matrizTwoGramEntidadesHoraErro","matrizTwoGramErros","matrizWikipedia","threeGram","treegram25","twoGDBPedia","twogram","twogram25","twoGram25Hora","twoGramCategoriaLocalizacao","twoGramDBPediaSubject","twoGramDiaSemana","twogramentidades","twoGramEntidades25","twoGramEntidadesHora","twoGramEntidadesHoraErro", "twoGramErros","vectorizerEstabelecimento","vectorizerHashTags","vocab","vocabEstabelecimento","vocabHashTags","wikipediaCategory")
  save.image(file="2110/rdas/compare22_resultado.RData")
}