library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

# get a list of the files in the input directory
files <- list.files("sentimento/presidentes")
files

GetSentiment <- function(file){
  # get the file
  fileName <- glue("sentimento/presidentes/", file, sep = "")
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # read in the new file
  fileText <- glue(read_file(fileName))
  # remove any dollar signs (they're special characters in R)
  fileText <- gsub("\\$", "", fileText) 
  
  # tokenize
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
    mutate(file = file) %>% # add the name of our file
    mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
    mutate(president = str_match(file, "(.*?)_")[2]) # add president
  
  # return our sentiment dataframe
  return(sentiment)
}

GetSentimentNrc <- function(file){
  # get the file
  fileName <- glue("sentimento/presidentes/", file, sep = "")
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # read in the new file
  fileText <- glue(read_file(fileName))
  # remove any dollar signs (they're special characters in R)
  fileText <- gsub("\\$", "", fileText) 
  
  # tokenize
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(get_sentiments("nrc")) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(file = file) %>% # add the name of our file
    mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
    mutate(president = str_match(file, "(.*?)_")[2]) # add president
  
  # return our sentiment dataframe
  return(sentiment)
}

# test: should return
# negative  positive    sentiment   file    year    president
# 117   240 123 Bush_1989.txt   1989    Bush

sentiments <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

sentiments

sentimentsNrc <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  sentimentsNrc <- rbind(sentimentsNrc, GetSentimentNrc(i))
}

sentimentsNrc


tes <- GetSentimentNrc(files[4])
tes$anger
tes$anticipation
tes$disgust
tes$fear
tes$joy
tes$negative
tes$positive
tes$sadness
tes$surprise
tes$trust

get_sentiments("nrc")



text <- "I’m not feeling good."

install.packages("devtools")
devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)
library(sentimentr)
mutate(sentiment = get_sentiment(text))


mytext <- c(
  'do you like it?  But I hate really bad dogs',
  'I am the best friend.',
  'Do you really like it?  I\'m not a fan'
)
mytext <- get_sentences(mytext)
sentiment(mytext)
