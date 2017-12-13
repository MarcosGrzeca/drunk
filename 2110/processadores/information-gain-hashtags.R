#options(java.parameters = "-Xmx32g")
options(java.parameters = "-Xmx90000m")
options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id, q1 AS resposta, hashtags FROM tweets t WHERE textparser <> '' AND id <> 462478714693890048")
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
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

it_train_hash = itoken(dados$hashtags, 
                       preprocessor = prep_fun, 
                       tokenizer = tok_fun, 
                       ids = dados$id, 
                       progressbar = TRUE)

vocabHashTags = create_vocabulary(it_train_hash)
vectorizerHashTags = vocab_vectorizer(vocabHashTags)
dtm_train_hash_tags = create_dtm(it_train_hash, vectorizerHashTags)

dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameHash)
maFinal <- subset(maFinal, select = -c(id, hashtags))

if (!require("FSelector")) {
  install.packages("FSelector")
}
library(FSelector)
weights <- information.gain(resposta~., maFinal)
weights
print(weights)
subset <- cutoff.k(weights, 47)
f <- as.simple.formula(subset, "resposta")
print(f)

dump(weights, "weights_hashtags.csv")

save.image(file="2110/rdas/information-gain-hashtags.RData")

load("2110/rdas/information-gain-hashtags.RData")
