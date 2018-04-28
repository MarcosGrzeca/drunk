#Somente funciona na versão antiga

#library(keras)
library(tools)
library(text2vec)

source(file_path_as_absolute("redesneurais/getDados.R"))
dados <- getDados()

tokens <- dados$textParser %>% tolower %>%  word_tokenizer

# create vocabulary
it = itoken(tokens)
stop_words <- tm::stopwords("en")
v <- create_vocabulary(it, stopwords = stop_words) %>% prune_vocabulary(term_count_min = 2)

vectorizer <- vocab_vectorizer(v)
#vectorizer = vocab_vectorizer(v, grow_dtm = F, skip_grams_window = 5)

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 100, vocabulary = v, x_max = 20)
glove$fit_transform(tcm, n_iter = 10)
glove$dump('glove.model')

#glove$save('glove.model')
#glove$dump('glove.model')
#word_vectors <- glove$get_word_vectors()
