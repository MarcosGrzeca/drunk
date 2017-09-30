options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id, q1 as resposta, textoParserEmoticom as textParser FROM tweets t")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
clearConsole()

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)
library(rowr)

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

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))

dataFramePresence <- convert_count(dataFrameTexto)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2013/bof.Rda")

maFinal <- cbind.fill(dados, dataFramePresence)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2013/bof_presence.Rda")