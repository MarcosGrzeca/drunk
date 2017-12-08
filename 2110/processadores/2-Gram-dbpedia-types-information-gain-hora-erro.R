options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id,
       t.idInterno,
       q1 AS resposta,
       textParser,
       textoParserEmoticom AS textoCompleto,
       hashtags,
       emoticonPos,
       emoticonNeg,
       hora,
       erroParseado as numeroErros,
  ( SELECT GROUP_CONCAT(DISTINCT(REPLACE(type, 'http://dbpedia.org/class/', '')))
   FROM
     ( SELECT c.resource AS resource,
              tn.idTweetInterno
      FROM tweets_nlp tn
      JOIN conceito c ON c.palavra = tn.palavra
      WHERE c.sucesso = 1
      UNION ALL SELECT c.resource AS resource,
                       tn.idTweetInterno
      FROM tweets_gram tn
      JOIN conceito c ON c.palavra = tn.palavra
      WHERE c.sucesso = 1
      GROUP BY 1,
               2 ) AS louco
     JOIN resource_type ty ON ty.resource = louco.resource
     WHERE louco.idTweetInterno = t.idInterno
     AND ty.type IN ('http://dbpedia.org/class/yago/Property104916342', 'http://dbpedia.org/class/yago/Manner104928903', 'http://dbpedia.org/class/yago/WikicatBeerStyles', 'http://dbpedia.org/class/yago/Attribute100024264', 'http://dbpedia.org/class/yago/Agent114778436', 'http://dbpedia.org/class/yago/Drug103247620', 'http://dbpedia.org/ontology/Beverage', 'http://dbpedia.org/class/yago/WikicatDrugs', 'http://dbpedia.org/class/yago/Carcinogen114793812', 'http://dbpedia.org/class/yago/WikicatDrugsActingOnTheNervousSystem', 'http://dbpedia.org/class/yago/WikicatIARCGroup1Carcinogens', 'http://dbpedia.org/class/yago/Substance100020090', 'http://dbpedia.org/ontology/Food', 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#FunctionalSubstance', 'http://www.wikidata.org/entity/Q2095', 'http://dbpedia.org/class/yago/Matter100020827', 'http://dbpedia.org/class/yago/Abstraction100002137', 'http://dbpedia.org/class/yago/DrugOfAbuse103248958', 'http://dbpedia.org/class/yago/Fluid114939900', 'http://dbpedia.org/class/yago/WikicatDistilledBeverages', 'http://dbpedia.org/class/yago/Liquid114940386', 'http://dbpedia.org/class/yago/CausalAgent100007347', 'http://dbpedia.org/class/yago/Beverage107881800', 'http://dbpedia.org/class/yago/Alcohol107884567', 'http://dbpedia.org/class/yago/Food100021265', 'http://www.w3.org/2002/07/owl#Thing', 'http://umbel.org/umbel/rc/AilmentCondition', 'http://dbpedia.org/class/yago/YagoLegalActorGeo', 'http://dbpedia.org/class/yago/YagoPermanentlyLocatedEntity', 'http://dbpedia.org/class/yago/Substance100019613', 'http://www.wikidata.org/entity/Q12136', 'http://dbpedia.org/ontology/Disease', 'http://dbpedia.org/class/yago/Location100027167', 'http://dbpedia.org/class/yago/Part113809207', 'http://dbpedia.org/class/yago/AdministrativeDistrict108491826', 'http://dbpedia.org/class/yago/YagoGeoEntity', 'http://dbpedia.org/class/yago/District108552138', 'http://dbpedia.org/class/yago/WikicatEthnicGroups', 'http://dbpedia.org/class/yago/Region108630985', 'http://dbpedia.org/ontology/EthnicGroup', 'http://dbpedia.org/class/yago/WikicatEthnicGroupsInTheUnitedStates', 'http://dbpedia.org/class/yago/WikicatEthnicGroupsInCanada', 'http://www.wikidata.org/entity/Q41710', 'http://dbpedia.org/class/yago/Wine107891726', 'http://dbpedia.org/class/yago/WikicatWineStyles', 'http://dbpedia.org/class/yago/SparklingWine107893528', 'http://dbpedia.org/class/yago/WikicatSparklingWines')
   ) AS resources
FROM tweets t")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
dados$resources <- enc2utf8(dados$resources)
dados$numeroErros[dados$numeroErros > 1] <- 1
dados <- discretizarHora(dados)
clearConsole()

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)

setDT(dados)
setkey(dados, id)

stem_tokenizer1 =function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="en")
}

dados$textParser = sub("'", "", dados$textParser)

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
                  #                  tokenizer = stem_tokenizer1,
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)


stop_words = tm::stopwords("en")
vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)

it_train_hash = itoken(dados$hashtags, 
                       preprocessor = prep_fun, 
                       tokenizer = tok_fun, 
                       ids = dados$id, 
                       progressbar = TRUE)

vocabHashTags = create_vocabulary(it_train_hash)
vectorizerHashTags = vocab_vectorizer(vocabHashTags)
dtm_train_hash_tags = create_dtm(it_train_hash, vectorizerHashTags)

it_train = itoken(strsplit(dados$resources, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameResource = create_dtm(it_train, vectorizer)
dataFrameResource <- as.data.frame(as.matrix(dataFrameResource))

#it_train = itoken(strsplit(dados$grams, ","), 
#                  ids = dados$id, 
                  #progressbar = TRUE)

#vocab = create_vocabulary(it_train)
#vectorizer = vocab_vectorizer(vocab)
#dataFrameGram = create_dtm(it_train, vectorizer)
#dataFrameGram <- as.data.frame(as.matrix(dataFrameGram))


#Concatenar resultados
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHash)
maFinal <- cbind.fill(maFinal, dataFrameResource)
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags, textoCompleto, resources))

save(maFinal, file = "2110/rdas/2-Gram-dbpedia-types-information-gain-hora-erro.Rda")


resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

#load("2110/rdas/compare22.RData")

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

CORES <- 7
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
  return (newdf)
}

library(magrittr)

set.seed(10)
split=0.80


load("2110/rdas/2-Gram-dbpedia-types-information-gain-hora-erro.Rda")
maFinal$resposta <- as.factor(maFinal$resposta)
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

twoGramTypesInfGainHoraErro <- treinar(data_train)
twoGramTypesInfGainHoraErro
matrizTwoGramTypesInfGainHoraErro <- getMatriz(twoGramTypesInfGainHoraErro, data_test)
resultados <- addRow(resultados, "2 Gram + Types (Information Gain) + Hora + Erro", matrizTwoGramTypesInfGainHoraErro)
save.image(file="2110/rdas/resultados_information_gain.RData")