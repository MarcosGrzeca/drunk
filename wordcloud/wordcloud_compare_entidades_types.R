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
               WHERE tn.tipo = 'C'
               UNION ALL
               SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
               FROM tweets_nlp tn
               JOIN tweets t ON tn.idTweetInterno = t.idInterno
               WHERE tn.tipo = 'K'
               UNION ALL
               SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
               FROM tweets_nlp tn
               JOIN tweets t ON tn.idTweetInterno = t.idInterno
               WHERE tn.tipo = 'CO'
               UNION ALL
               SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
               FROM tweets_nlp tn
               JOIN tweets t ON tn.idTweetInterno = t.idInterno
               WHERE tn.tipo = 'E'
               ")

dados$entidades <- enc2utf8(dados$entidades)
dados$entidades <- iconv(dados$entidades, to='ASCII//TRANSLIT')
dados$entidades = gsub(" ", "_", dados$entidades)
dados$entidades = gsub("---", " ", dados$entidades)
#dados$entidades = gsub("/", " ", dados$entidades)

nrow(dados)

library(tm)
library(dplyr)
library(xtable)

dados$entidades

docs <- Corpus(VectorSource(dados$entidades)) %>%
  tm_map(removePunctuation) %>%
  # tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)
  #%>% tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(docs) %>%
  removeSparseTerms(0.8)  %>%
  as.matrix()

colnames(tdm) <- c("Categories","Key words","Concepts","Entities")

comparison.cloud(tdm, random.order=FALSE,
                 title.size=2, max.words=400)

warnings()
#tdmMatrix <- as.matrix(tfNeg)
#dados$entidades
#paste(unlist(dados$entidades), collapse =". ")