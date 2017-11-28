options(max.print = 99999999)

library(text2vec)
library(data.table)
data("movie_review")
setDT(movie_review)
setkey(movie_review, id)
set.seed(2017L)
all_ids = movie_review$id
train_ids = sample(all_ids, 4000)
test_ids = setdiff(all_ids, train_ids)
train = movie_review[J(train_ids)]
test = movie_review[J(test_ids)]


prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$review, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)

testeM
testeM <- word_tokenizer(train$review)
teste
View(teste)

require(reshape2)
teste$id <- rownames(teste) 
melt(teste)

teste <- space_tokenizer(train$review, " ")


df<-data.frame(words = unlist(words))

df<-data.frame(testeM = unlist(testeM))

transform(voacv)

it_train

vocab$stopwords

text2vec_vocabulary

View(vocab$ngram)

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

corpus <- create_corpus(it_train, vectorizer)
View(corpus)

corpus
vectorizer
vocab
unlist(vocab)
df <- data.frame(vocab = unlist(vocab))
dump(df, "words.csv")



results <- lapply(vocab,
                  function(x) {
                    print(x)
                    }
)

it_test = test$review %>% 
  prep_fun %>% 
  tok_fun %>% 
  itoken(ids = test$id, 
         # turn off progressbar because it won't look nice in rmd
         progressbar = FALSE)

dtm_test = create_dtm(it_test, vectorizer)

dump(dtm_train, "vocab.csv")

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tfidf)
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf  = create_dtm(it_test, vectorizer) %>% 
  transform(tfidf)


View(dtm_test_tfidf)


dump(dtm_train_tfidf, "vocab.csv")

dtm_train_tfidf


library("tm") # version 0.6, you seem to be using an older version
data(crude)
revs <- tm_map(crude, content_transformer(tolower)) 
revs <- tm_map(revs, removeWords, stopwords("english")) 
revs <- tm_map(revs, removePunctuation) 
revs <- tm_map(revs, removeNumbers) 
revs <- tm_map(revs, stripWhitespace) 
dtm <- DocumentTermMatrix(revs)
dtm

rowSums()

as.matrix(dtm)

data(crude, package="tm")
mycorpus <- corpus(crude)
summary(mycorpus)


library(dplyr)
library(tidytext)
library(janeaustenr)

train$review

stop_words = tm::stopwords("en")

austen_bigrams <- train %>%
  unnest_tokens(unigram, review, token = "ngrams", n = 1) %>%
  filter(!unigram %in% stop_words)

marcos <- austen_bigrams %>%
  count(unigram, sort = TRUE)

dump(marcos,"teste.csv")


aa <- as.data.frame(marcos)
dump(aa,"teste2.csv")