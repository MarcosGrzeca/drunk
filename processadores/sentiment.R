#Carregar bibliotecas
options(max.print = 99999999)

#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm-2016"
clearConsole();
dadosQ1 <- query("SELECT id, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags FROM tweets WHERE situacao = 'N'")

dados <- dadosQ1

if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr)

sentiments <- sentiment_by(dados$textoCompleto)
dados$sentiment <- sentiments$ave_sentiment


sentimentsHash <- sentiment_by(dados$hashtags)
dados$sentimentH <- sentimentsHash$ave_sentiment

dados$sentimentHdados$emotiom <- 0
dados$emotiom[sentiments$ave_sentiment < -0.5] <- -2
dados$emotiom[sentiments$ave_sentiment < 0] <- -1
dados$emotiom[sentiments$ave_sentiment > 0] <- 1
dados$emotiom[sentiments$ave_sentiment > 0.5] <- 2

sentiments <- sentiment_by(dados$hashtags)
dados$hashEmo <- 0
dados$hashEmo[sentiments$ave_sentiment < -0.5] <- -2
dados$hashEmo[sentiments$ave_sentiment < 0] <- -1
dados$hashEmo[sentiments$ave_sentiment > 0] <- 1
dados$hashEmo[sentiments$ave_sentiment > 0.5] <- 2

fore <- function(x) {
  query(paste("UPDATE `tweets` SET sentiment = ", x[2], ", sentimentH = ", x[3], " WHERE id = ", x[1], "", sep=""));
}
apply(subset(dados, select = c(id, sentiment, sentimentH)), 1, fore)