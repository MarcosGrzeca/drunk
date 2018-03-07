library(tidytext)

#https://www.tidytextmining.com/tidytext.html

sentiments

get_sentiments("afinn")

nrc #TIPOS
bing #positivo e negativo
AFINN #indice de polaridade

get_sentiments("nrc")

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

tidy_books


nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


text_df %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df <- data_frame(line = 1:4, text = text)
text_df

library(tidytext)

text_df %>%
  unnest_tokens(word, text)


austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

m %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

m
m %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

remove.packages(c("exploratory"))
