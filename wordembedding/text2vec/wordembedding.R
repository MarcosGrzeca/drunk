#http://dsnotes.com/post/glove-enwiki/

#https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html
library(text2vec)
#data("movie_review")

options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT textoParserRisadaEmoticom FROM tweets t")

tokens = dados$textoParserRisadaEmoticom %>% tolower %>%  word_tokenizer
it = itoken(tokens)
# create vocabulary
v = create_vocabulary(it) %>% prune_vocabulary(term_count_min = 1)
# create co-occurrence vectorizer
vectorizer = vocab_vectorizer(v, grow_dtm = F, skip_grams_window = 5)
#vectorizer = vocab_vectorizer(v, grow_dtm = F)
it = itoken(tokens)
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = v, x_max = 10)
glove$fit(tcm, n_iter = 20)
word_vectors <- glove$get_word_vectors()

berlin <- word_vectors["paris", , drop = FALSE] - word_vectors["france", , drop = FALSE] + word_vectors["germany", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)


berlin2 <- word_vectors["drink", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin2, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

