options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"

dados <- query("SELECT t.id, q1 as resposta, textParser FROM tweets t")
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)

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
                  #tokenizer = stem_tokenizer1,
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)

it_train_stem = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
                  tokenizer = stem_tokenizer1,
                  #tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)

stop_words = tm::stopwords("en")
vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 1L))
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))

library(rowr)
maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2014/bof_uni.Rda")

vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2014/bof_bi.Rda")

vocab = create_vocabulary(it_train_stem, stopwords = stop_words)
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train_stem, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2014/bof_stem_uni.Rda")

vocab = create_vocabulary(it_train_stem, stopwords = stop_words, ngram = c(1L, 2L))
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train_stem, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2014/bof_stem_bi.Rda")