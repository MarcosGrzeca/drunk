options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

#dadosQ1 <- query("SELECT t.id, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags, emoticonPos, emoticonNeg, sentiment, sentimentH, localCount, organizationCount, moneyCount, personCount, numeroErros, numeroConjuncoes, taxaSubstantivo, taxaAdjetivo, taxaAdverbio, taxaVerbo, palavroes, hora, tl.name as nomeEstabelecimento, tl.category as categoriaEstabelecimento, diaSemana FROM tweets t LEFT JOIN tweet_localizacao tl ON tl.idTweetInterno = t.idInterno AND distance = 100")

#SQL com types
dadosQ1 <- query("SELECT t.id, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags, emoticonPos, emoticonNeg, sentiment, sentimentH, localCount, organizationCount, moneyCount, personCount, erroParseado as numeroErros, numeroConjuncoes, taxaSubstantivo, taxaAdjetivo, taxaAdverbio, taxaVerbo, palavroes, hora, tl.name as nomeEstabelecimento, tl.category as categoriaEstabelecimento, diaSemana, (SELECT GROUP_CONCAT(ty.type) FROM tweets_nlp tn JOIN conceito c ON c.palavra = tn.palavra JOIN resource_type ty ON ty.resource = c.resource WHERE tn.idTweetInterno = t.idInterno AND ty.`type` IN ('http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#SocialPerson', 'http://schema.org/Organization', 'http://dbpedia.org/ontology/Group', 'http://schema.org/MusicGroup', 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#NaturalPerson', 'http://schema.org/Product', 'http://dbpedia.org/class/yago/Rocket104099429', 'http://dbpedia.org/class/yago/Holder110180178', 'http://dbpedia.org/ontology/Artist', 'http://dbpedia.org/class/yago/Female109619168', 'http://dbpedia.org/class/yago/Child109918248', 'http://dbpedia.org/class/yago/Male109624168', 'http://dbpedia.org/class/yago/Document103217458', 'http://dbpedia.org/class/yago/AthleticFacility102752311', 'http://dbpedia.org/class/yago/Pool103982060', 'http://dbpedia.org/class/yago/PlasticArt103958097', 'http://dbpedia.org/class/yago/SolidFigure113863473', 'http://dbpedia.org/class/yago/Attacker109821253', 'http://dbpedia.org/class/yago/AcousticDevice102676261', 'http://dbpedia.org/class/yago/SignalingDevice104217718', 'http://dbpedia.org/class/yago/Anomaly114505821', 'http://dbpedia.org/class/yago/Defect114464005', 'http://dbpedia.org/class/yago/Climber113102409', 'http://dbpedia.org/class/yago/ComputerUser109951274', 'http://dbpedia.org/class/yago/Engineer109615807', 'http://dbpedia.org/class/yago/RootVegetable107710283', 'http://dbpedia.org/class/yago/SolanaceousVegetable107710007', 'http://dbpedia.org/class/yago/Stimulant104320126', 'http://dbpedia.org/class/yago/Ceramic102997391', 'http://dbpedia.org/class/yago/Biome107941945') GROUP by t.id ) as entidades, (SELECT GROUP_CONCAT(ty.type) FROM tweets_gram tn JOIN conceito c ON c.palavra = tn.palavra JOIN resource_type ty ON ty.resource = c.resource WHERE tn.idTweetInterno = t.idInterno AND ty.`type` IN ('http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#SocialPerson', 'http://schema.org/Organization', 'http://dbpedia.org/ontology/Group', 'http://schema.org/MusicGroup', 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#NaturalPerson', 'http://schema.org/Product', 'http://dbpedia.org/class/yago/Rocket104099429', 'http://dbpedia.org/class/yago/Holder110180178', 'http://dbpedia.org/ontology/Artist', 'http://dbpedia.org/class/yago/Female109619168', 'http://dbpedia.org/class/yago/Child109918248', 'http://dbpedia.org/class/yago/Male109624168', 'http://dbpedia.org/class/yago/Document103217458', 'http://dbpedia.org/class/yago/AthleticFacility102752311', 'http://dbpedia.org/class/yago/Pool103982060', 'http://dbpedia.org/class/yago/PlasticArt103958097', 'http://dbpedia.org/class/yago/SolidFigure113863473', 'http://dbpedia.org/class/yago/Attacker109821253', 'http://dbpedia.org/class/yago/AcousticDevice102676261', 'http://dbpedia.org/class/yago/SignalingDevice104217718', 'http://dbpedia.org/class/yago/Anomaly114505821', 'http://dbpedia.org/class/yago/Defect114464005', 'http://dbpedia.org/class/yago/Climber113102409', 'http://dbpedia.org/class/yago/ComputerUser109951274', 'http://dbpedia.org/class/yago/Engineer109615807', 'http://dbpedia.org/class/yago/RootVegetable107710283', 'http://dbpedia.org/class/yago/SolanaceousVegetable107710007', 'http://dbpedia.org/class/yago/Stimulant104320126', 'http://dbpedia.org/class/yago/Ceramic102997391', 'http://dbpedia.org/class/yago/Biome107941945') GROUP by t.id ) as grams FROM tweets t LEFT JOIN tweet_localizacao tl ON tl.idTweetInterno = t.idInterno AND distance = 100")

#SQL com category
dadosQ1 <- query("SELECT t.id, q1 AS resposta, textParser, textoParserEmoticom AS textoCompleto, hashtags, emoticonPos, emoticonNeg, sentiment, sentimentH, localCount, organizationCount, moneyCount, personCount, erroParseado AS numeroErros, numeroConjuncoes, taxaSubstantivo, taxaAdjetivo, taxaAdverbio, taxaVerbo, palavroes, hora, tl.name AS nomeEstabelecimento, tl.category AS categoriaEstabelecimento, diaSemana, (SELECT GROUP_CONCAT(replace(ty.subject, 'http://dbpedia.org/resource/Category:', '')) FROM tweets_nlp tn JOIN conceito c ON c.palavra = tn.palavra JOIN resource_subject ty ON ty.resource = c.resource WHERE tn.idTweetInterno = t.idInterno AND ty.escolhida = 1 GROUP BY t.id) AS entidades, (SELECT GROUP_CONCAT(replace(ty.subject, 'http://dbpedia.org/resource/Category:', '')) FROM tweets_gram tn JOIN conceito c ON c.palavra = tn.palavra JOIN resource_subject ty ON ty.resource = c.resource WHERE tn.idTweetInterno = t.idInterno AND escolhida = 1 GROUP BY t.id) AS grams FROM tweets t LEFT JOIN tweet_localizacao tl ON tl.idTweetInterno = t.idInterno AND distance = 100")

dados <- dadosQ1
dados$resposta[is.na(dados$resposta)] <- 0
dados$numeroErros[dados$numeroErros > 1] <- 1
dados$palavroes[dados$palavroes > 1] <- 1
dados$resposta <- as.factor(dados$resposta)
clearConsole()

dados <- discretizarTaxas(dados)
dados <- discretizarHora(dados)
dados <- discretizarSentimentos(dados);

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
#vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 3L))
vocab = create_vocabulary(it_train, stopwords = stop_words)
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

dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))

dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()


#Parsear categoria do estabelecimento
dados$categoriaEstabelecimento = sub(" ", "_", dados$categoriaEstabelecimento)
it_train = itoken(dados$categoriaEstabelecimento, 
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)
vocabEstabelecimento = create_vocabulary(it_train, stopwords = stop_words)
vocabEstabelecimento = prune_vocabulary(vocabEstabelecimento, 
                                term_count_min = 5, 
                                doc_proportion_max = 0.9,
                                doc_proportion_min = 0.001)
vectorizerEstabelecimento = vocab_vectorizer(vocabEstabelecimento)
dataFrameEstabelecimento = create_dtm(it_train, vectorizerEstabelecimento)
dataFrameEstabelecimento <- as.data.frame(as.matrix(dataFrameEstabelecimento))

it_train = itoken(strsplit(dados$entidades, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameEntidades = create_dtm(it_train, vectorizer)
dataFrameEntidades <- as.data.frame(as.matrix(dataFrameEntidades))

it_train = itoken(strsplit(dados$grams, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameGram = create_dtm(it_train, vectorizer)
dataFrameGram <- as.data.frame(as.matrix(dataFrameGram))
#CERTO

library(rowr)
library(RWeka)

cols <- colnames(dataFrameTexto)
aspectos <- sort(colSums(dataFrameTexto), decreasing = TRUE)
manter <- round(length(aspectos) * 0.25)
aspectosManter <- c()
aspectosRemover <- c()

for(i in 1:length(aspectos)) {
  if (i <= manter) {
    aspectosManter <- c(aspectosManter, aspectos[i])
  } else {
    aspectosRemover <- c(aspectosRemover, aspectos[i])
  }
}

dataFrameTexto <- dataFrameTexto[names(aspectosManter)]

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHash)
#maFinal <- cbind.fill(maFinal, dataFrameEstabelecimento)
maFinal <- cbind.fill(maFinal, dataFrameEntidades)
maFinal <- cbind.fill(maFinal, dataFrameGram)
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags, textoCompleto, categoriaEstabelecimento, nomeEstabelecimento, entidades, grams))

save(maFinal, file = "dados_1609_category.Rda")

load("dados_2008_end_hora.Rda")
maFinal <- subset(maFinal, select = -c(nomeEstabeleciomento))
maFinal <- subset(maFinal, select = -c(localCount, organizationCount, moneyCount, personCount, numeroConjuncoes, palavroes, adjetivo, substantivo, adverbio, verbo, turno, emotiom, emotiomH, diaSemana))

maFinal <- subset(maFinal, select = -c(numeroErros))

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(16)

set.seed(10)
split=0.80
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

print("Treinando")
fit <- train(x = subset(data_train, select = -c(resposta)),
             y = data_train$resposta, 
             method = "svmLinear", 
             #method = "svmPoly", 
             trControl = trainControl(method = "cv", number = 5, savePred=T),
             #,preProc=c("center", "scale", "nzv")
             preProc=c("center")
             #na.action = na.omit
)
fit

library(mlbench)
pred <- predict(fit, subset(data_test, select = -c(resposta)))
matriz <- confusionMatrix(data = pred, data_test$resposta, positive="1")
matriz
matriz$byClass["F1"]
matriz$byClass["Precision"]
matriz$byClass["Recall"]

#fitPadrao <- fit

load("tentando.Rda")
library(caret)

resamps <- resamples(list("3-GRAM, #, Emoticon, CV" = fitTrigram,
                          "BoW, #, Emoticon, CV" = fitBagWords,
                          "BoW, #, Emoticon, Sentiment, Sentiment #, CV" = fitBagWordsSentiment,
                          "BoW, #, Emoticon, Sentiment, Sentiment #, NLP, CV" = fitCompletao,
                          "3-GRAM, #, Emoticon, Stemming, CV" = fitStemming,
                          "BoW, #, Emoticon, Sentiment, Sentiment #, NLP" = fitSemCV))

summary(resamps)

bwplot(resamps, layout = c(1, 2))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Accuracy")

trellis.par.set(theme1)
xyplot(resamps, what = "BlandAltman")

difValues <- diff(resamps)
trellis.par.set(theme1)
bwplot(difValues, layout = c(3, 1))

dotplot(difValues)

save.image(file="image_2008_2.RData")

teste <- c(0.017194343099999999,0.024730841699999999,0.052694374000000002,0.0003649635,0.0003649635,0.010482212600000001,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0091976734000000001,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0019160583999999999,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.031386861299999999,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.00067518250000000004,0.0052819343,0.0003649635,0.0028467153000000002,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0037773722999999999,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0057162408999999999,0.0108077099,0.0019160583999999999,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.00098540150000000007,0.0024434307000000001,0.0064281933999999997,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.00067518250000000004,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0065693430999999997,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.010376505499999999,0.00067518250000000004,0.0046614964000000004,0.0003649635,0.00067518250000000004,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.00052007300000000002,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0040875911999999999,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635,0.0003649635)


teste <- c(0.01719433, 0.02473084, 0.05269436, 0.00036496, 0.00036496, 0.01048221, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00919766, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00191606, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.03138686, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00067518, 0.00528192, 0.00036496, 0.00284672, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00377736, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00571624, 0.01080771, 0.00191606, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00098539, 0.00244342, 0.00642819, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00067518, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00656933, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.01037651, 0.00067518, 0.00466149, 0.00036496, 0.00067518, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00052006, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00408759, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496, 0.00036496)
