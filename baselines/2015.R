options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"

dados <- query("SELECT t.id, q1 as resposta, textParser FROM tweets t WHERE textParser <> '' AND id <> 462478714693890048")
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)

setDT(dados)
setkey(dados, id)

dados$textParser = sub("'", "", dados$textParser)

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)

stop_words = tm::stopwords("en")
vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 1L))
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFramePresence <- convert_count(dataFrameTexto)

library(rowr)
maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2015/bof_uni.Rda")

maFinal <- cbind.fill(dados, dataFramePresence)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2015/bof_uni_presence.Rda")

vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFramePresence <- convert_count(dataFrameTexto)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2015/bof_bi.Rda")

maFinal <- cbind.fill(dados, dataFramePresence)
maFinal <- subset(maFinal, select = -c(textParser, id))
save(maFinal, file = "baselines/dataset/2015/bof_bi_presence.Rda")


require(MASS)
r <- lda(formula = resposta ~ ., 
         data = maFinal,
         CV = TRUE)

r
#lda(maFinal$resposta, maFinal)


library(topicmodels)
#colSums(dataFrameTexto)
#a <- rowSums(dataFrameTexto)
#View(b)

ap_lda <- LDA(dataFrameTexto, k = 20, control = list(seed = 1234))
library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")

library(dplyr)

ammp_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
View(ammp_top_terms)

dump(ammp_top_terms, "baselines/dataset/2015/neg.csv")

library(ggplot2)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()