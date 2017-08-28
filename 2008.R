options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm-2016"
clearConsole();

dadosQ1 <- query("SELECT t.id, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags, emoticonPos, emoticonNeg, sentiment, sentimentH, localCount, organizationCount, moneyCount, personCount, numeroErros, numeroConjuncoes, taxaSubstantivo, taxaAdjetivo, taxaAdverbio, taxaVerbo, palavroes, hora, IFNULL(tl.name, 0) as nomeEstabeleciomento, IFNULL(tl.category, 0) as categoriaEstabelecimento FROM tweets t LEFT JOIN tweet_localizacao tl ON tl.idTweetInterno = t.idInterno AND distance = 75")

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

dataTexto <- as.matrix(dtm_train_texto)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()

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
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags, textoCompleto))

save(maFinal, file = "dados_2708_bow.Rda")

#dump(maFinal, "dados_2708_end_hora.csv");
#load("dadosLiq.Rda")

FILE <- "exp1_completao.Rda"

load("dados_2008_end_hora.Rda")
maFinal <- subset(maFinal, select = -c(adverbio))


#maFinal$mention
#maFinal$url

load("dados_2708_end_hora_ttt.Rda")

#maFinal <- subset(maFinal, select = -c(nomeEstabeleciomento))
#maFinal$categoriaEstabelecimento <- as.factor(maFinal$categoriaEstabelecimento)
maFinal$categoriaEstabelecimento[maFinal$categoriaEstabelecimento == "0"] <- NA
#maFinal <- subset(maFinal, select = -c(localCount, organizationCount, moneyCount, personCount, numeroErros, numeroConjuncoes, palavroes, nomeEstabeleciomento, categoriaEstabelecimento, adjetivo, substantivo, adverbio, verbo, turno, emotiom, emotiomH))


library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(8)

set.seed(10)
split=0.80
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

print("Treinando")
fit <- train(x = subset(data_train, select = -c(resposta)),
             y = data_train$resposta, 
             method = "svmRadial", 
             #method = "svmPoly", 
             trControl = trainControl(method = "cv", number = 5, savePred=T),
             #,preProc=c("center", "scale", "nzv")
             preProc=c("center")
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

DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA), z = c(4,3,5))
na.pass(DF)

library(e1071)


tuned = tune.svm(data_train$resposta, data = subset(data_train, select = -c(resposta)), gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=10))
