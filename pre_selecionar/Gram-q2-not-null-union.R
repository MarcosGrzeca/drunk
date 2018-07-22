options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("
  SELECT  t.id,
          q2 AS resposta,
          textParser,
          hashtags,
          erroParseado as numeroErros,
          emoticonPos,
          emoticonNeg,
          hora
  FROM tweets t
  WHERE textparser <> ''
  AND id <> 462478714693890048
  AND q1 = 1
  AND q2 IS NOT NULL
  UNION ALL
  SELECT id,
        q2 AS resposta,
        textParser,
        hashtags,
        erros as numeroErros,
        emoticonPos,
        emoticonNeg,
        hora
  FROM semantic_tweets
  WHERE q1 = 1
  AND q2 IS NOT NULL
  ")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
dados$textParser <- iconv(dados$textParser, to='ASCII//TRANSLIT')
dados$textParser = gsub("'", "", dados$textParser)
dados$hashtags = gsub("#", "hashtag__", dados$hashtags)
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

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)

stop_words = tm::stopwords("en")
vocab = create_vocabulary(it_train, stopwords = stop_words)
vocab = prune_vocabulary(vocab, term_count_min = 2)

vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)


it_train_hash = itoken(dados$hashtags, 
                       preprocessor = prep_fun, 
                       tokenizer = tok_fun, 
                       ids = dados$id, 
                       progressbar = TRUE)

vocabHashTags = create_vocabulary(it_train_hash)
vocabHashTags = prune_vocabulary(vocabHashTags, term_count_min = 2)

vectorizerHashTags = vocab_vectorizer(vocabHashTags)
dtm_train_hash_tags = create_dtm(it_train_hash, vectorizerHashTags)

#Concatenar resultados
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHash)
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags))

save(maFinal, file = "pre_selecionar/gram-q2-not-null-union.Rda")