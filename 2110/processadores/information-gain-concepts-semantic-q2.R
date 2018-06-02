#options(java.parameters = "-Xmx32g")
options(java.parameters = "-Xmx90000m")
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
     GROUP BY tn.idTweetInterno) AS entidades
FROM tweets t
WHERE q1 = 1 
    AND q2 IS NOT NULL")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$entidades <- enc2utf8(dados$entidades)
dados$entidades <- iconv(dados$entidades, to='ASCII//TRANSLIT')

clearConsole()

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)
registerDoMC(8)

setDT(dados)
setkey(dados, id)

stem_tokenizer1 =function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="en")
}

prep_fun = tolower
tok_fun = word_tokenizer

stop_words = tm::stopwords("en")

it_train = itoken(strsplit(dados$entidades, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameResource = create_dtm(it_train, vectorizer)
dataFrameResource <- as.data.frame(as.matrix(dataFrameResource))

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameResource)
maFinal <- subset(maFinal, select = -c(id, entidades))

if (!require("FSelector")) {
  install.packages("FSelector")
}
library(FSelector)
weights <- information.gain(resposta~., maFinal)
print(weights)
subset <- cutoff.k(weights, 100)
f <- as.simple.formula(subset, "resposta")
print(f)

dump(weights, "planilhas/q2")

save.image(file="2110/rdas/information-gain-concept-semantic.RData")