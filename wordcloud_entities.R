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

dados <- query("SELECT 
               (SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---')
                FROM tweets_nlp tn
                WHERE tn.idTweetInterno = t.idInterno
                GROUP BY tn.idTweetInterno) AS entidades
               FROM tweets t
               WHERE q1 = 0")

dados$entidades <- enc2utf8(dados$entidades)
dados$entidades <- iconv(dados$entidades, to='ASCII//TRANSLIT')
dados$entidades = gsub(" ", "_", dados$entidades)
dados$entidades = gsub("---", " ", dados$entidades)

stop_words = tm::stopwords("en")

library(wordcloud)
require(tm)

corp = Corpus(VectorSource(dados$entidades)) 
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
#corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, function(x)removeWords(x,stopwords('english')))
wordcloud(corp, max.words=100)

wordcloud(corp, scale=c(2.5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))



#term.matrix <- TermDocumentMatrix(corp)
#term.matrix <- as.matrix(term.matrix)
#term.matrix

#Word cloud por tfidf

#creating wordcloud
#packages required:- tm,wordcloud
corpus=Corpus(VectorSource(dados$entidades))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
#corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,removeWords,stopwords("en"))
corpus=Corpus(VectorSource(corpus))
tdm=TermDocumentMatrix(corpus)
m=as.matrix(tdm)
v=sort(rowSums(m),decreasing=T)
d=data.frame(words=names(v),freq=v)

wordcloud(d$words,d$freq,max.words=100,colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
