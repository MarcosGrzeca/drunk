options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id,
       q2 AS resposta,
       textParser,
       textoParserEmoticom AS textoCompleto,
       hashtags,
       emoticonPos,
       emoticonNeg,
       hora,
       erroParseado as numeroErros,
    (SELECT GROUP_CONCAT(tn.palavra)
     FROM tweets_nlp tn
     WHERE tn.idTweetInterno = t.idInterno
     GROUP BY tn.idTweetInterno) AS entidades,
    '0' as testador
    FROM tweets t
    WHERE textparser <> ''
    AND id <> 462478714693890048
    AND q1 = 1
    AND q2 IS NOT NULL
    
    UNION 
    
    SELECT id,
        0 AS resposta,
        textParser,
        textOriginal as textoCompleto,
        hashtags,
        emoticonPos,
        emoticonNeg,
        hora,
        erros,
        (
          SELECT GROUP_CONCAT(DISTINCT(tn.palavra))
          FROM semantic_tweets_nlp tn
          WHERE tn.idTweet = t.id
          -- AND tn.tipo NOT IN ('language', 'socialTag')
          GROUP BY tn.idTweet
        ) AS entidades,
        '1'
        FROM semantic_tweets_alcolic t
        WHERE situacao = 1
        AND possuiURL = 0
        AND LENGTH(textOriginal) > 5
        AND indicativoDrunk = 1
    ")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
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

dados$textParser = sub("'", "", dados$textParser)

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
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

it_train = itoken(strsplit(dados$entidades, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameEntidades = create_dtm(it_train, vectorizer)
dataFrameEntidades <- as.data.frame(as.matrix(dataFrameEntidades))

#Concatenar resultados
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHash)
maFinal <- cbind.fill(maFinal, dataFrameEntidades)
maFinal <- subset(maFinal, select = -c(textParser, hashtags, textoCompleto, entidades))

save(maFinal, file = "2110/rdas/2gram-entidades-hora-erro-q2-teste-outro-classificador.Rda")
