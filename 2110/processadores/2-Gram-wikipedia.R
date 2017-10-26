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
    (
     SELECT GROUP_CONCAT(DISTINCT(REPLACE(category, 'Category:', '')))
     FROM 
     (
       SELECT c.resource as resource, tn.idTweetInterno
       FROM tweets_nlp tn
       JOIN conceito c ON c.palavra = tn.palavra
       WHERE c.sucesso = 1
       UNION ALL
       SELECT c.resource as resource, tn.idTweetInterno
       FROM tweets_gram tn
       JOIN conceito c ON c.palavra = tn.palavra
       WHERE c.sucesso = 1
       GROUP BY 1,2
     ) as louco
     JOIN wikipedia_category wc ON louco.resource = wc.resource
       WHERE louco.idTweetInterno = t.idInterno
     ) as resources FROM tweets t")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
dados$resources <- enc2utf8(dados$resources)
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

save(maFinal, file = "2110/rdas/2gram-wikipedia.Rda")
