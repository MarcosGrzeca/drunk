#http://dsnotes.com/post/glove-enwiki/

library(session)


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
stop_words = tm::stopwords("en")
v = create_vocabulary(it, stopwords = stop_words) %>% prune_vocabulary(term_count_min = 1)

# create co-occurrence vectorizer
vectorizer = vocab_vectorizer(v, grow_dtm = F, skip_grams_window = 5)
#vectorizer = vocab_vectorizer(v, grow_dtm = F)
it = itoken(tokens)
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = v, x_max = 10)
glove$fit(tcm, n_iter = 10)
word_vectors <- glove$get_word_vectors()

#berlin <- word_vectors["paris", , drop = FALSE] - word_vectors["france", , drop = FALSE] + word_vectors["germany", , drop = FALSE]
#cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
#head(sort(cos_sim[,1], decreasing = TRUE), 5)


#cos_sim = sim2(x = word_vectors, y = word_vectors["tasting", , drop = FALSE], method = "cosine", norm = "l2")
#head(sort(cos_sim[,1], decreasing = TRUE), 10)

#palavras <- c("drinking", "tequila", "wine", "vodka", "alcohol", "drunk", "liquor", "brew", "tour", "ale", "booze", "tasting", "cold", "alcoholic", "drink", "drinks", "hammered", "ipa", "beer", "champagne", "bud", "rum", "crawl", "brewing", "pong")
#palavras

#results <- lapply(palavras,
#    function(x) {
      #grep(as.character(x), pattern=paste0("\\<",word))
#      cos_sim = sim2(x = word_vectors, y = word_vectors[x, , drop = FALSE], method = "cosine", norm = "l2")
#      head(sort(cos_sim[,1], decreasing = TRUE), 10)
#    }
#)

#results
#dump(as.data.frame(results), "embedding.csv")

save.image("model.Rda")
save.session(file="session.Rda")