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
dados <- query("SELECT textoParserEmoticom FROM tweets t wHERE q1 = 1")
stop_words = tm::stopwords("en")

library(wordcloud)
require(tm)

corp = Corpus(VectorSource(dados$textoParserEmoticom)) 
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, function(x)removeWords(x,stopwords('english')))
wordcloud(corp, max.words=100)

wordcloud(corp, scale=c(2.5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))



#term.matrix <- TermDocumentMatrix(corp)
#term.matrix <- as.matrix(term.matrix)
#term.matrix

#Word cloud por tfidf

#creating wordcloud
#packages required:- tm,wordcloud
corpus=Corpus(VectorSource(dados$textoParserEmoticom))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,removeWords,stopwords("en"))
corpus=Corpus(VectorSource(corpus))
tdm=TermDocumentMatrix(corpus)
m=as.matrix(tdm)
v=sort(rowSums(m),decreasing=T)
d=data.frame(words=names(v),freq=v)
wordcloud(d$words,d$freq,max.words=100,colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
