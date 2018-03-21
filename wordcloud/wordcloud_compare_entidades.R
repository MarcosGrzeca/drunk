library(wordcloud)
library(tidyverse)
library(tidytext)
library(text2vec)
library(dplyr)
library(tidytext)
library(janeaustenr)

library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
               FROM tweets_nlp tn
               JOIN tweets t ON tn.idTweetInterno = t.idInterno
               WHERE q1 = 1
               UNION
               SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
               FROM tweets_nlp tn
               JOIN tweets t ON tn.idTweetInterno = t.idInterno
               WHERE q1 = 0")

dados$entidades <- enc2utf8(dados$entidades)
dados$entidades <- iconv(dados$entidades, to='ASCII//TRANSLIT')
dados$entidades = gsub(" ", "_", dados$entidades)
dados$entidades = gsub("---", " ", dados$entidades)
dados$entidades = gsub("/", " ", dados$entidades)

stop_words = tm::stopwords("en")

library(tm)
library(dplyr)
library(xtable)

docs <- Corpus(VectorSource(dados$entidades)) %>%
  tm_map(removePunctuation) %>%
  # tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) <- c("Alcohol","Sober")

par(mfrow=c(1,1))
comparison.cloud(tdm, random.order=FALSE, colors = c("indianred3","blue3"),
                 title.size=2, max.words=200)

warnings()
#tdmMatrix <- as.matrix(tfNeg)
#dados$entidades
#paste(unlist(dados$entidades), collapse =". ")