options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"

dados <- query("SELECT t.id, q1 as resposta, textParser, textoParserEmoticom, taxaAdjetivo, taxaSubstantivo, taxaAdverbio, taxaVerbo, numeroConjuncoes, erroParseado as numeroErros, (SELECT COUNT(DISTINCT(tn.palavra)) FROM tweets_nlp tn WHERE idTweetInterno = idInterno) as entidades, possuiEmoticon, (qtdPos + qtdNeg) as totalEmoticons, sentiment, sentimentH FROM tweets t WHERE textParser <> '' AND id <> 462478714693890048")

library(stringr)
library(rowr)

repetidos <- function(x) {
  y <- sum(str_count(x[2], c("aaa", "bbb", "ccc", "ddd", "eee", "fff", "ggg", "hhh", "iii", "jjj", "kkk", "lll", "mmm", "nnn", "ooo", "ppp", "qqq", "rrr", "sss", "ttt", "uuu", "vvv", "www", "xxx", "yyy", "zzz")))
  y <- ifelse(y > 0, 1,0)
  y
}

countUpper <- function(x) {
  cont <- sum(str_count(x[2], c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")))
  cont
}

repet <- apply(subset(dados, select = c(id, textoParserEmoticom)), 1, repetidos)
repet <- as.data.frame(repet)
cont <- apply(subset(dados, select = c(id, textoParserEmoticom)), 1, countUpper)
cont <- as.data.frame(cont)
numeroPal <- sapply(gregexpr("\\W+", dados$textParser), length) + 1
numeroPal <- as.data.frame(numeroPal)

dados$numeroErros[dados$numeroErros > 1] <- 1
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)

dados <- discretizarTaxas(dados)
dados <- discretizarSentimentos(dados);

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
vocab = create_vocabulary(it_train, stopwords = stop_words)
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))

dataFrameTexto <- subset(dataFrameTexto, select = c("1", "12", "1st", "2", "24", "4", "5", "6", "7", "alcohol", "ale", "alone", "always", "amp", "ass", "athletic", "away", "b", "baby", "back", "bad", "ball", "bar", "beer", "best", "better", "big", "binge", "birthday", "bitch", "bitches", "bottle", "bottles", "boys", "brew", "bring", "can", "cant", "cause", "check", "club", "cold", "come", "country", "crazy", "cream", "crown", "damn", "dance", "day", "dinosaur", "don", "dont", "drink", "drinking", "drunk", "eat", "end", "even", "ever", "every", "everybody", "family", "feel", "fit", "food", "friend", "friends", "fuck", "fucked", "fucking", "game", "geneseo", "get", "getting", "girl", "girls", "go", "going", "golf", "gonna", "good", "got", "grad", "great", "gt", "haha", "hangover", "happy", "hit", "home", "hope", "house", "ice", "ill", "im", "ipa", "irish", "isnt", "ive", "join", "june", "just", "know", "last", "lean", "let", "life", "like", "liquor", "literally", "lmao", "lol", "lot", "love", "m", "mad", "made", "make", "making", "man", "media", "mention", "mom", "morning", "much", "music", "n", "need", "never", "new", "night", "nothing", "now", "ny", "oh", "one", "others", "party", "people", "perfect", "phone", "play", "pong", "probably", "pub", "put", "que", "real", "really", "right", "rochester", "root", "s", "said", "saturday", "say", "school", "screen", "see", "shit", "shot", "show", "someone", "something", "still", "store", "strip", "summer", "syracuse", "t", "take", "tea", "thank", "thats", "think", "tho", "time", "today", "tomorrow", "tonight", "trying", "turn", "turnt", "turnup", "two", "u", "url", "us", "video", "vodka", "w", "wait", "wanna", "want", "wasted", "water", "way", "weekend", "welcome", "well", "whats", "whole", "will", "wine", "women", "world", "ya", "yak", "yet", "yo", "youre"))
dataFramePresence <- convert_count(dataFrameTexto)

library(rowr)

maFinal <- cbind.fill(dados, dataFrameTexto, repet, cont, numeroPal)
maFinal <- subset(maFinal, select = -c(textParser, id, textoParserEmoticom, emotiomH))
save(maFinal, file = "baselines/dataset/2015/stile_gram.Rda")

maFinal <- cbind.fill(dados, dataFramePresence, repet, cont, numeroPal)
maFinal <- subset(maFinal, select = -c(textParser, id, textoParserEmoticom, emotiomH))
save(maFinal, file = "baselines/dataset/2015/stile_gram_presence.Rda")