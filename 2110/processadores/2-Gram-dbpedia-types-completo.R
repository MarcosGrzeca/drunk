options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id,
       q1 AS resposta,
       textParser,
       textoParserEmoticom AS textoCompleto,
       hashtags,
       emoticonPos,
       emoticonNeg,
    (SELECT GROUP_CONCAT(ty.type)
     FROM tweets_nlp tn
     JOIN conceito c ON c.palavra = tn.palavra
     JOIN resource_type ty ON ty.resource = c.resource
     WHERE tn.idTweetInterno = t.idInterno
     GROUP BY t.id) AS entidades,
    (SELECT GROUP_CONCAT(ty.type)
     FROM tweets_gram tn
     JOIN conceito c ON c.palavra = tn.palavra
     JOIN resource_type ty ON ty.resource = c.resource
     WHERE tn.idTweetInterno = t.idInterno
         AND ty.`type` IN ('http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#SocialPerson',
                           'http://schema.org/Organization',
                           'http://dbpedia.org/ontology/Group',
                           'http://schema.org/MusicGroup',
                           'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#NaturalPerson',
                           'http://schema.org/Product',
                           'http://dbpedia.org/class/yago/Rocket104099429',
                           'http://dbpedia.org/class/yago/Holder110180178',
                           'http://dbpedia.org/ontology/Artist',
                           'http://dbpedia.org/class/yago/Female109619168',
                           'http://dbpedia.org/class/yago/Child109918248',
                           'http://dbpedia.org/class/yago/Male109624168',
                           'http://dbpedia.org/class/yago/Document103217458',
                           'http://dbpedia.org/class/yago/AthleticFacility102752311',
                           'http://dbpedia.org/class/yago/Pool103982060',
                           'http://dbpedia.org/class/yago/PlasticArt103958097',
                           'http://dbpedia.org/class/yago/SolidFigure113863473',
                           'http://dbpedia.org/class/yago/Attacker109821253',
                           'http://dbpedia.org/class/yago/AcousticDevice102676261',
                           'http://dbpedia.org/class/yago/SignalingDevice104217718',
                           'http://dbpedia.org/class/yago/Anomaly114505821',
                           'http://dbpedia.org/class/yago/Defect114464005',
                           'http://dbpedia.org/class/yago/Climber113102409',
                           'http://dbpedia.org/class/yago/ComputerUser109951274',
                           'http://dbpedia.org/class/yago/Engineer109615807',
                           'http://dbpedia.org/class/yago/RootVegetable107710283',
                           'http://dbpedia.org/class/yago/SolanaceousVegetable107710007',
                           'http://dbpedia.org/class/yago/Stimulant104320126',
                           'http://dbpedia.org/class/yago/Ceramic102997391',
                           'http://dbpedia.org/class/yago/Biome107941945')
     GROUP BY t.id) AS resources
FROM tweets t")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
dados$resources <- enc2utf8(dados$resources)
clearConsole()

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)

setDT(dados)
setkey(dados, id)

stem_tokenizer1 =function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="en")
}

dados$textParser = sub("'", "", dados$textParser)

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
                  #                  tokenizer = stem_tokenizer1,
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)


stop_words = tm::stopwords("en")
vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)

it_train_hash = itoken(dados$hashtags, 
                       preprocessor = prep_fun, 
                       tokenizer = tok_fun, 
                       ids = dados$id, 
                       progressbar = TRUE)

vocabHashTags = create_vocabulary(it_train_hash)
vectorizerHashTags = vocab_vectorizer(vocabHashTags)
dtm_train_hash_tags = create_dtm(it_train_hash, vectorizerHashTags)

it_train = itoken(strsplit(dados$resources, ","), 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dataFrameResource = create_dtm(it_train, vectorizer)
dataFrameResource <- as.data.frame(as.matrix(dataFrameResource))

#it_train = itoken(strsplit(dados$grams, ","), 
#                  ids = dados$id, 
                  #progressbar = TRUE)

#vocab = create_vocabulary(it_train)
#vectorizer = vocab_vectorizer(vocab)
#dataFrameGram = create_dtm(it_train, vectorizer)
#dataFrameGram <- as.data.frame(as.matrix(dataFrameGram))


#Concatenar resultados
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHash)
maFinal <- cbind.fill(maFinal, dataFrameResource)
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags, textoCompleto, resources))

save(maFinal, file = "2110/rdas/2-Gram-dbpedia-types-completo.Rda")