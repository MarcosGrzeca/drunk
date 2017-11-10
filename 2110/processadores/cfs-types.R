#options(java.parameters = "-Xmx32g")
options(java.parameters = "-Xmx39000m")
options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id,
               q1 AS resposta,
               ( SELECT GROUP_CONCAT(DISTINCT(REPLACE(type, 'http://dbpedia.org/class/', '')))
               FROM
               ( SELECT c.resource AS resource,
               tn.idTweetInterno
               FROM tweets_nlp tn
               JOIN conceito c ON c.palavra = tn.palavra
               WHERE c.sucesso = 1
               UNION ALL SELECT c.resource AS resource,
               tn.idTweetInterno
               FROM tweets_gram tn
               JOIN conceito c ON c.palavra = tn.palavra
               WHERE c.sucesso = 1
               GROUP BY 1,
               2 ) AS louco
               JOIN resource_type ty ON ty.resource = louco.resource
               WHERE louco.idTweetInterno = t.idInterno
               ) AS resources
               FROM tweets t")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
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

prep_fun = tolower
tok_fun = word_tokenizer

stop_words = tm::stopwords("en")

it_train = itoken(strsplit(dados$resources, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameResource = create_dtm(it_train, vectorizer)
dataFrameResource <- as.data.frame(as.matrix(dataFrameResource))

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameResource)
maFinal <- subset(maFinal, select = -c(id, resources))

if (!require("FSelector")) {
  install.packages("FSelector")
}
library(FSelector)
subset <- cfs(resposta~., maFinal)
subset
f <- as.simple.formula(subset, "resposta")
print(f)

save.image(file="2110/rdas/cfs.RData")
