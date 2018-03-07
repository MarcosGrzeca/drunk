library(tools)
source(file_path_as_absolute("functions.R"))

DATABASE <- "icwsm"
clearConsole();
dados <- query("SELECT id, idInterno, textoParserEmoticom as textoCompleto FROM tweets")

library(doMC)
library(rowr)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(NLP)

processador <- function(x) {
  
  #texto <- as.String("MARCOS")
  texto <- x[2]
  
  print(texto)
  
  texto <- gsub("\\$", "", texto)
  #tokens <- data_frame(text = texto) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  #sentiment <- tokens %>%
  #  inner_join(get_sentiments("nrc")) %>% # pull out only sentimen words
  #  count(sentiment) %>% # count the # of positive & negative words
  #  spread(sentiment, n, fill = 0)# made data wide rather than narrow
  
  sentiment <- get_nrc_sentiment(texto)

  anger <- 0
  anticipation <- 0
  disgust <- 0
  fear <- 0
  joy <- 0
  sadness <- 0
  surprise <- 0
  trust <- 0
  negative <- 0
  positive <- 0

  if("anger" %in% colnames(sentiment)) {
    anger <- sentiment$anger
  }
  if("anticipation" %in% colnames(sentiment)) {
    anticipation <- sentiment$anticipation
  }
  if("disgust" %in% colnames(sentiment)) {
    disgust <- sentiment$disgust
  }
  if("fear" %in% colnames(sentiment)) {
    fear <- sentiment$fear
  }
  if("joy" %in% colnames(sentiment)) {
    joy <- sentiment$joy
  }
  if("sadness" %in% colnames(sentiment)) {
    sadness <- sentiment$sadness
  }
  if("surprise" %in% colnames(sentiment)) {
    surprise <- sentiment$surprise
  }
  if("trust" %in% colnames(sentiment)) {
    trust <- sentiment$trust
  }
  if("negative" %in% colnames(sentiment)) {
    negative <- sentiment$negative
  }
  if("positive" %in% colnames(sentiment)) {
    positive <- sentiment$positive
  }  
  
  sqla <-paste("INSERT INTO `tweets_sentiment_type` VALUES (", x[1], ", ", anger, ", ", anticipation, ", ", disgust, ", ", fear, ", ", joy, ", ", sadness, ", ", surprise, ", ", trust, ", ", negative, ", ", positive, ")", sep="");
  query(sqla)
  return (1)
}

apply(subset(dados, select = c(idInterno, textoCompleto)), 1, processador)