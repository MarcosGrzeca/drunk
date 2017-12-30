options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id,
       CONCAT('MM', textParser) as textParser,
       textoParserEmoticom AS textoCompleto,
       hashtags,
       emoticonPos,
       emoticonNeg,
      q1 AS resposta,

    (SELECT GROUP_CONCAT(CONCAT('eeee_', tn.palavra))
     FROM tweets_nlp tn
     WHERE tn.idTweetInterno = t.idInterno
     AND origem = 'A'
     AND tipo = 'C'
     GROUP BY tn.idTweetInterno) AS entidades
FROM tweets t
WHERE textparser <> ''
    AND id <> 462478714693890048
LIMIT 500
    ")
#AND q1 IS NOT NULL

dados$textParser

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
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

dados$textParser = gsub("'", "", dados$textParser)
dados$textParser = gsub(":", " ", dados$textParser)
dados$textParser = gsub("#", "", dados$textParser)
dados$textParser = gsub("-", " ", dados$textParser)
dados$textParser = gsub(" ", " MM", dados$textParser)
dados$textParser
dados$entidades = gsub(" ", "_", dados$entidades)
dados$entidades = gsub("/", "..", dados$entidades)

dados$hashtags = gsub("#", "#tag_", dados$hashtags)

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

#it_train_hash = itoken(dados$hashtags, 
#                       preprocessor = prep_fun, 
#                       tokenizer = tok_fun, 
#                       ids = dados$id, 
#                       progressbar = TRUE)

it_train_hash = itoken(dados$hashtags, 
                       preprocessor = prep_fun, 
                       tokenizer = tok_fun, 
                       ids = dados$id, 
                       progressbar = TRUE)

vocabHashTags = create_vocabulary(it_train_hash)
vectorizerHashTags = vocab_vectorizer(vocabHashTags)
dtm_train_hash_tags = create_dtm(it_train_hash, vectorizerHashTags)


it_train = itoken(strsplit(dados$entidades, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameEntidades = create_dtm(it_train, vectorizer)
dataFrameEntidades <- as.data.frame(as.matrix(dataFrameEntidades))

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

#maFinal <- cbind.fill(dataFrameTexto, dataFrameHash)
#maFinal <- cbind.fill(maFinal, dataFrameEntidades)

#maFinal <- cbind.fill(dataFrameTexto, dataFrameEntidades)
#maFinal <- cbind.fill(maFinal, subset(dados, select = -c(textParser, id, hashtags, textoCompleto, entidades)))

#maFinal <- cbind.fill(dataFrameHash, dataFrameEntidades)
maFinal <- cbind.fill(dataFrameTexto, subset(dados, select = -c(textParser, id, hashtags, textoCompleto, entidades)))
save(maFinal, file = "2110/rdas/2gram-entidades-alchemy-categories-not-null-resposta.Rda")

colnames(dataFrameTexto)
