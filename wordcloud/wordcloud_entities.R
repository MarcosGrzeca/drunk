library(wordcloud)
library(tidyverse)
library(tidytext)
library(text2vec)
library(dplyr)
library(janeaustenr)
require(tm)

library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm"

parseCorpus <- function (tipo) {
  if (tipo == 1) {
    dados <- query("SELECT 
                 (SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---')
                  FROM tweets_nlp tn
                  WHERE tn.idTweetInterno = t.idInterno
                  GROUP BY tn.idTweetInterno) AS entidades
                 FROM tweets t
                 WHERE q1 = 1")
  } else {
    dados <- query("SELECT 
                 (SELECT GROUP_CONCAT(tn.palavra SEPARATOR '---')
                  FROM tweets_nlp tn
                  WHERE tn.idTweetInterno = t.idInterno
                  GROUP BY tn.idTweetInterno) AS entidades
                 FROM tweets t
                 WHERE q1 = 0")
  }
  
  dados$entidades <- enc2utf8(dados$entidades)
  dados$entidades <- iconv(dados$entidades, to='ASCII//TRANSLIT')
  dados$entidades = gsub(" ", "_", dados$entidades)
  dados$entidades = gsub("---", " ", dados$entidades)
  #dados$entidades = gsub("/", " ", dados$entidades)

  stop_words = tm::stopwords("en")

  corp = Corpus(VectorSource(dados$entidades)) 
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, content_transformer(tolower))
  #corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, function(x)removeWords(x,stopwords('english')))
  
  #corpus = Corpus(VectorSource(corpus))
  #tdm = TermDocumentMatrix(corpus)
  #m = as.matrix(tdm)
  #v = sort(rowSums(m),decreasing=T)
  #dneg = data.frame(words=names(v),freq=v)
  
  return (corp);
}

par(mfrow=c(1,2))
#wordcloud(parseCorpus(0), scale=c(2.5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
#wordcloud(parseCorpus(1), scale=c(2.5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
#wordcloud(d$words,d$freq,max.words=100,colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(parseCorpus(0), scale=c(2,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(parseCorpus(1), scale=c(2,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


#wordcloud(parseCorpus(1), scale=c(1,0.5), max.words=10, random.order=TRUE, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
