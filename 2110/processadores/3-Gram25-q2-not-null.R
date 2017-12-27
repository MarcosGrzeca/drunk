options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id, q2 AS resposta, textParser, textoParserEmoticom AS textoCompleto, hashtags, emoticonPos,	emoticonNeg FROM tweets t WHERE textparser <> '' AND id <> 462478714693890048 AND q1 = 1 AND q2 IS NOT NULL")
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
dados$hashtags <- enc2utf8(dados$hashtags)
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

save(maFinal, file = "2110/rdas/3gram-25-q2-not-null.Rda")