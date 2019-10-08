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

dados <- query("SELECT GROUP_CONCAT(entidades SEPARATOR '---') as entidades
                FROM (
                  SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
                  FROM tweets_nlp tn
                  JOIN tweets t ON tn.idTweetInterno = t.idInterno
                  WHERE q2 = 1
                  AND tipo NOT IN ('socialTag', 'language')
                  UNION ALL
                  SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
                  FROM tweets_amazon_nlp tn
                  JOIN tweets_amazon t ON tn.idTweet = t.id
                  WHERE q2 = 1
                  AND tipo NOT IN ('socialTag', 'language')
                ) as X
                UNION
                SELECT GROUP_CONCAT(entidades SEPARATOR '---') as entidades
                FROM (
                  SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
                  FROM tweets_nlp tn
                  JOIN tweets t ON tn.idTweetInterno = t.idInterno
                  AND tipo NOT IN ('socialTag', 'language')
                  WHERE q1 = 0
                  UNION ALL
                  SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---') as entidades
                  FROM tweets_amazon_nlp tn
                  JOIN tweets_amazon t ON tn.idTweet = t.id
                  WHERE q2 = 0
                  AND tipo NOT IN ('socialTag', 'language')
                ) as Y"
                )

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
colnames(tdm) <- c("Drunk","Sober")

par(mfrow=c(1,1))
comparison.cloud(tdm, random.order=FALSE, colors = c("indianred3","blue3"),
                 title.size=2, max.words=200)

warnings()
#tdmMatrix <- as.matrix(tfNeg)
#dados$entidades
#paste(unlist(dados$entidades), collapse =". ")

# png("#102_1_comparison_cloud_top_2000_words.png", width = 480, height = 480)
# comparison.cloud(document_tm_clean_mat_s,max.words=2000,random.order=FALSE,c(4,0.4), title.size=1.4)
# dev.off()