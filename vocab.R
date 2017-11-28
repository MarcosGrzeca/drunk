#http://tidytextmining.com/ngrams.html

library(text2vec)

options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT textoParserRisadaEmoticom FROM tweets t wHERE q1 = 0")

library(dplyr)
library(tidytext)
library(janeaustenr)

stop_words = tm::stopwords("en")

austen_bigrams <- dados %>%
  unnest_tokens(unigram, textoParserRisadaEmoticom, token = "ngrams", n = 1) %>%
  filter(!unigram %in% stop_words)

marcos <- austen_bigrams %>%
  count(unigram, sort = TRUE)

dump(marcos,"q1_negativo.csv")

#aa <- as.data.frame(marcos)
#dump(aa,"teste2.csv")