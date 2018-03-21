library(wordcloud)
library(tidyverse)
library(tidytext)
library(text2vec)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(wordcloud)
require(tm)

library(tools)
source(file_path_as_absolute("functions.R"))
DATABASE <- "icwsm"

parseCorpus <- function (tipo) {
  clearConsole();
  if (tipo == 1) {
    dados <- query("SELECT textoParserEmoticom FROM tweets t wHERE q1 = 1")
  } else {
    dados <- query("SELECT textoParserEmoticom FROM tweets t wHERE q1 = 0")
  }
  stop_words = tm::stopwords("en")

  #creating wordcloud
  #packages required:- tm,wordcloud
  corpus=Corpus(VectorSource(dados$textoParserEmoticom))
  corpus=tm_map(corpus,tolower)
  corpus=tm_map(corpus,removePunctuation)
  corpus=tm_map(corpus,removeNumbers)
  corpus=tm_map(corpus,removeWords,stopwords("en"))
  
  #corpus=Corpus(VectorSource(corpus))
  #tdm=TermDocumentMatrix(corpus)
  #m=as.matrix(tdm)
  #v=sort(rowSums(m),decreasing=T)
  #d=data.frame(words=names(v),freq=v)
  
  return (corpus);
}

CorpusPositive <- parseCorpus(1)
CorpusNegative <- parseCorpus(0)

par(mfrow=c(1,2))
#wordcloud(d$words,d$freq,max.words=100,colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F,title="Positive words")
wordcloud(CorpusNegative, max.words=100,colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(CorpusPositive, max.words=100,colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)