options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm-2016"
clearConsole();

#dadosQ1 <- query("SELECT id, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags, emoticonPos, emoticonNeg FROM tweets")
dadosQ1 <- query("SELECT id, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags, emoticonPos, emoticonNeg, sentiment, sentimentH, localCount, organizationCount, moneyCount, personCount, numeroErros, numeroConjuncoes, taxaSubstantivo, taxaAdjetivo, taxaAdverbio, taxaVerbo, palavroes FROM tweets WHERE situacao = 'S'")
dados <- dadosQ1

dados$resposta[is.na(dados$resposta)] <- 0
dados$numeroErros[dados$numeroErros > 1] <- 1
dados$palavroes[dados$palavroes > 1] <- 1
dados$resposta <- as.factor(dados$resposta)
clearConsole()

discretizarTaxas <- function(dados) {
  dados$adjetivo <- 0
  dados$adjetivo[dados$taxaAdjetivo > 0.20] <- 1
  dados$adjetivo[dados$taxaAdjetivo > 0.40] <- 2
  dados$adjetivo[dados$taxaAdjetivo > 0.60] <- 3
  dados$adjetivo[dados$taxaAdjetivo > 0.80] <- 4
  
  dados$substantivo <- 0
  dados$substantivo[dados$taxaSubstantivo > 0.15] <- 1
  dados$substantivo[dados$taxaSubstantivo > 0.30] <- 2
  dados$substantivo[dados$taxaSubstantivo > 0.45] <- 3
  dados$substantivo[dados$taxaSubstantivo > 0.60] <- 4
  dados$substantivo[dados$taxaSubstantivo > 0.75] <- 5
  dados$substantivo[dados$taxaSubstantivo > 0.90] <- 6
  
  dados$adverbio <- 0
  dados$adverbio[dados$taxaAdverbio > 0.17] <- 1
  dados$adverbio[dados$taxaAdverbio > 0.34] <- 2
  dados$adverbio[dados$taxaAdverbio > 0.51] <- 3
  dados$adverbio[dados$taxaAdverbio > 0.68] <- 4
  
  dados$verbo <- 0
  dados$verbo[dados$taxaVerbo > 0.17] <- 1
  dados$verbo[dados$taxaVerbo > 0.34] <- 2
  dados$verbo[dados$taxaVerbo > 0.51] <- 3
  dados$verbo[dados$taxaVerbo > 0.68] <- 4
  return (dados)
}

dados <- discretizarTaxas(dados)

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
vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 3L))
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

discretizarSentimentos <- function(dados) {
  #sentimentos
  dados$emotiom <- 0
  dados$emotiom[dados$sentiment < 0] <- -1
  dados$emotiom[dados$sentiment < -0.33] <- -2
  dados$emotiom[dados$sentiment < -0.66] <- -3
  dados$emotiom[dados$sentiment > 0] <- 1
  dados$emotiom[dados$sentiment > 0.33] <- 2
  dados$emotiom[dados$sentiment > 0.66] <- 3
  
  dados$emotiomH <- 0
  dados$emotiomH[dados$sentimentH < 0] <- -1
  dados$emotiomH[dados$sentimentH < -0.5] <- -2
  dados$emotiomH[dados$sentimentH > 0] <- 1
  dados$emotiomH[dados$sentimentH > 0.5] <- 2
  return (dados)
}

dados <- discretizarSentimentos(dados);

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
maFinal <- subset(maFinal, select = -c(sentiment, sentimentH))
maFinal <- subset(maFinal, select = -c(taxaAdjetivo, taxaAdverbio, taxaSubstantivo, taxaVerbo))
save(maFinal, file = "dados_2008.Rda")

aa <- colnames(maFinal)
aa[1:50]

#load("dadosLiq.Rda")

FILE <- "exp1_completao.Rda"

load("dados_2008.Rda")

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
             method = "svmLinear", 
             trControl = trainControl(method = "cv", number = 5, savePred=T)
             #,preProc=c("center", "scale", "nzv")
             ,preProc=c("center")
) 
fit

library(mlbench)
pred <- predict(fit, subset(data_test, select = -c(resposta)))
matriz <- confusionMatrix(data = pred, data_test$resposta, positive="1")
matriz
matriz$byClass["Precision"]
matriz$byClass["Recall"]
matriz$byClass["F1"]

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
