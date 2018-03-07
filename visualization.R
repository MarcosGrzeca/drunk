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
